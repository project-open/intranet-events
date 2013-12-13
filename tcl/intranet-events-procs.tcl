# /packages/intranet-events/tcl/intranet-events-procs.tcl
#
# Copyright (C) 2003-2013 ]project-open[


ad_library {
    Definitions for events

    @author frank.bergmann@project-open.com
}

# ---------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------

ad_proc -public im_event_type_default {} { return 82000 }

ad_proc -public im_event_status_unplanned {} { return 82000 }
ad_proc -public im_event_status_planned {} { return 82002 }
ad_proc -public im_event_status_reserved {} { return 82004 }
ad_proc -public im_event_status_booked {} { return 82006 }
ad_proc -public im_event_status_deleted {} { return 82099 }

# Status for each event participant
ad_proc -public im_event_participant_status_confirmed {} { return 82200 }
ad_proc -public im_event_participant_status_reserved {} { return 82210 }
ad_proc -public im_event_participant_status_deleted {} { return 82290 }



ad_proc -public im_profile_consultants {} {
     return [im_profile::profile_id_from_name -profile "Consultants"]
}


namespace eval im_event {

# ----------------------------------------------------------------------
# Event - Timesheet Task Sweeper
# ---------------------------------------------------------------------

    ad_proc -public task_sweeper {
	{-sweep_mode "full"}
	{-event_id ""}
	{-debug_p 1}
    } {
        Periodic sweeper that executes task_sweeper_helper.
        @author frank.bergmann@project-open.com
    } {
	ns_log Notice "im_event::task_sweeper: Starting"
	# Make sure that only one thread is sweeping at a time
	if {[nsv_incr intranet_events sweeper_p] > 1} {
	    nsv_incr intranet_events sweeper_p -1
	    ns_log Notice "im_event::task_sweeper: Aborting - there is another process running"
	    return "busy"
	}

	# Call the actual sweeper
	set debug_html ""
	if {[catch {
	    set debug_html [task_sweeper_helper \
		-sweep_mode $sweep_mode \
		-event_id $event_id \
		-debug_p $debug_p \
	    ]
	} err_msg]} {
	    ad_return_complaint 1 "<pre>$err_msg</pre>"
	    append debug_html "error:\n$err_msg"
	}

	# Debugging - wait for 5 seconds
	# exec sleep 5

	# Free the semaphore for next use
	nsv_incr intranet_events sweeper_p -1
	ns_log Notice "im_event::task_sweeper: Finished"
	return $debug_html
    }

    ad_proc -public task_sweeper_helper {
	{-sweep_mode ""}
	{-event_id ""}
	{-debug_p 1}
    } {
        Sweeper "helper" doing the actual work. 
	Checks that every event is represented by a timesheet task 
	with the same members, so that event trainers can log their 
	hours.

	@param full_sweep_p Normally sweeps only affect events
	       without task or recently modified events.
        @author frank.bergmann@project-open.com
    } {
	ns_log Notice "im_event::task_sweeper_helper: sweep_mode=$sweep_mode, event_id=$event_id, debug_p=$$debug_p"
	set debug ""

	# -------------------------------------------------
	# Determine which events to sweep

	# Default: Sweep only recently modified events or 
	# completely dirty ones (no timesheet task entry)
	set sweep_select_sql "
		select	e.event_id
		from	im_events e,
			acs_objects o
		where	e.event_id = o.object_id and
			(	-- no task yet
				event_timesheet_task_id is null
			OR	-- never swept yet
				event_timesheet_last_swept is null
			OR	-- modified since last sweep
				o.last_modified > event_timesheet_last_swept
			OR	-- members have changed
				e.event_member_list_cached != im_biz_object_member__list(e.event_id)
			)
	"
	# Full sweep - sweep all events, which may take minutes...
	if {"full" == $sweep_mode} {
	    set sweep_select_sql "select e.event_id from im_events e"
	}

	# Sweep only a specific event
	if {"" != $event_id && [string is integer $event_id]} { 
	    set sweep_select_sql "select e.event_id from im_events e where e.event_id = $event_id" 
	}


	set sweep_sql "
		select	e.*,
			im_biz_object_member__list(e.event_id) as event_member_list
		from	im_events e
		where	e.event_id in ($sweep_select_sql)
        "
	# -------------------------------------------------
        # Check the situation of relevant events
	db_foreach sweep_events $sweep_sql {
	    lappend debug "task_sweeper_helper: Sweeping event# $event_id"

	    # Do we need to re-index the event in order to keep the 
	    # TSearch2 index up to date?
	    if {$event_member_list_cached != $event_member_list} {
		db_dml update-reindex-tsearch "update im_events set event_member_list_cached = :event_member_list where event_id = :event_id"
	    }

	    # The year for the event - depends on it's start date
	    set year [string range $event_start_date 0 3]

	    # Make sure the parent project exists
	    set parent_project_id [db_string parent_project_id "
		select	project_id
		from	im_projects
		where	project_nr = :year || '_events'
	    " -default ""]
	    if {"" == $parent_project_id} {
		lappend debug "task_sweeper_helper: Did not find project '${year}_events' - creating a new one"
		set parent_project_id [im_project::new \
					   -project_name       "$year Events" \
					   -project_nr         "${year}_events" \
					   -project_path       "${year}_events" \
					   -company_id         [im_company_internal] \
					   -parent_id          "" \
					   -project_type_id    [im_project_type_consulting] \
					   -project_status_id  [im_project_status_open] \
		]
		db_dml update_parent_project "
			update im_projects set
			    	start_date = to_date(:year || '-01-01', 'YYYY-MM-DD'),
				end_date = to_date(:year || '-12-31', 'YYYY-MM-DD')
			where project_id = :parent_project_id
		"
	    }
	    lappend debug "task_sweeper_helper: Event parent project# $parent_project_id"


	    # -----------------------------------------------------
	    # Set all important task fields
	    set task_nr "$event_nr"
	    set task_name "$event_name"

	    set project_id $parent_project_id
	    set material_id $event_material_id
	    if {"" == $material_id} { set material_id [im_material_default_material_id] }
	    set cost_center_id $event_cost_center_id
	    set uom_id [im_uom_hour]
	    set task_type_id [im_project_type_task]
	    set note $event_description
	    set planned_units 0
	    set billable_units 0
	    set percent_completed ""

	    switch $event_status_id {
		82004 {
		    # Reserved
		    set task_status_id [im_project_status_potential]
		}
		82002  {
		    # Planned
		    set task_status_id [im_project_status_potential]
		}
		82006  {
		    # Booked
		    set task_status_id [im_project_status_open]
		}
		182511  {
		    # Squared
		    set task_status_id [im_project_status_open]
		}
		default {
		    set task_status_id [im_project_status_open]
		}
	    }


	    # -----------------------------------------------------
	    # Create or update the timesheet task
	    
	    # Check if this event doesn't yet have a task associated
	    if {"" == $event_timesheet_task_id} {
		# Check if the task already exists
		set event_timesheet_task_id [db_string task_id "
		    	select	p.project_id
			from	im_projects p,
				im_timesheet_tasks t
			where	p.project_id = t.task_id and
				p.parent_id = :parent_project_id and
				p.project_nr = :task_nr
	        " -default ""]
		lappend debug "task_sweeper_helper: Found task #$event_timesheet_task_id for task_nr='$task_nr'"
	    }

	    if {"" == $event_timesheet_task_id} {
		lappend debug "task_sweeper_helper: About to create a new task"
		set event_timesheet_task_id [db_string task_insert {}]
		lappend debug "task_sweeper_helper: Newly created task #$event_timesheet_task_id"
	    }
	    db_dml task_update {}
	    db_dml project_update {}
	    db_dml update_event "
		update im_events
		set event_timesheet_task_id = :event_timesheet_task_id
		where event_id = :event_id
	    "

	    # -----------------------------------------------------
	    # Copy event members to task
	    set event_member_sql "
		select	object_id_two as user_id,
			bom.object_role_id as role_id,
			bom.percentage
		from	acs_rels r,
			im_biz_object_members bom
		where	r.rel_id = bom.rel_id and
			object_id_one = :event_id and
			object_id_two in (select member_id from group_distinct_member_map where group_id = [im_profile_employees])
	    "
	    array set event_member_hash {}
	    db_foreach event_members $event_member_sql {
		im_biz_object_add_role -percentage $percentage $user_id $event_timesheet_task_id $role_id
		set event_member_hash($user_id) $role_id
	    }

	    # -----------------------------------------------------
	    # Remove task members who are not event members
	    set task_member_sql "
		select	object_id_two as user_id,
			bom.object_role_id as role_id
		from	acs_rels r,
			im_biz_object_members bom
		where	r.rel_id = bom.rel_id and
			object_id_one = :event_timesheet_task_id
	    "
	    array set task_member_hash {}
	    db_foreach task_members $task_member_sql {
		set task_member_hash($user_id) $role_id
	    }

	    foreach uid [array names task_member_hash] {
		if {![info exists event_member_hash($uid)]} {
		    db_string delete_membership "
		    	select im_biz_object_member__delete(:event_timesheet_task_id, :uid)
		    "
		}
	    }

	    # Marks as swept
	    db_dml update_event "
		update im_events
		set event_timesheet_last_swept = now()
		where event_id = :event_id
	    "

	    # Write Audit Trail
	    im_project_audit -user_id 0 -project_id $event_timesheet_task_id -action after_create
	}
	return [join $debug "\n"]
    }

    # ----------------------------------------------------------------------
    # Generate unique event_nr
    # ---------------------------------------------------------------------

    ad_proc -public next_event_nr {
    } {
        Create a new event_nr. Calculates the max() of current
	event_nrs and add +1, or just use a sequence for the next value.

        @author frank.bergmann@project-open.com
	@return next event_nr
    } {
	set next_event_nr_method [parameter::get_from_package_key -package_key "intranet-events" -parameter "NextEventNrMethod" -default "sequence"]

	switch $next_event_nr_method {
	    sequence {
		# Make sure everybody _really_ gets a different NR!
		return [db_nextval im_event_nr_seq]
	    }
	    default {
		# Try to avoid any "holes" in the list of event NRs
		set last_event_nr [db_string last_pnr "
		select	max(event_nr::integer)
		from	im_events
		where	event_nr ~ '^\[0-9\]+$'
	        " -default 0]

		# Make sure the counter is not behind the current value
		while {[db_string last_value "select im_event_nr_seq.last_value from dual"] < $last_event_nr} {
		    db_dml update "select nextval('im_event_seq')"
		}
		return [expr $last_event_nr + 1]
	    }
	}
    }
}



# ---------------------------------------------------------------------
# Event Customer Component
# ---------------------------------------------------------------------

ad_proc im_event_customer_component { event_id form_mode plugin_id view_name orderby return_url } {
    Returns a formatted HTML showing the event customers, customer members and related order items
} {
    set params [list \
                    [list event_id $event_id] \
                    [list form_mode $form_mode] \
                    [list plugin_id $plugin_id] \
                    [list view_name $view_name] \
                    [list orderby $orderby] \
                    [list return_url $return_url] \
    ]

    set result [ad_parse_template -params $params "/packages/intranet-events/lib/event-customers"]
    return [string trim $result]
}



ad_proc im_event_order_item_component { event_id form_mode plugin_id view_name orderby return_url } {
    Returns a formatted HTML showing the event order_items
} {
    set params [list \
                    [list event_id $event_id] \
                    [list form_mode $form_mode] \
                    [list plugin_id $plugin_id] \
                    [list view_name $view_name] \
                    [list orderby $orderby] \
                    [list return_url $return_url] \
    ]

    set result [ad_parse_template -params $params "/packages/intranet-events/lib/event-order-items"]
    return [string trim $result]
}


ad_proc im_event_participant_component { event_id form_mode plugin_id view_name orderby return_url } {
    Returns a formatted HTML showing the event participants, participant members and related order items
} {
    set params [list \
                    [list event_id $event_id] \
                    [list form_mode $form_mode] \
                    [list plugin_id $plugin_id] \
                    [list view_name $view_name] \
                    [list orderby $orderby] \
                    [list return_url $return_url] \
    ]

    set result [ad_parse_template -params $params "/packages/intranet-events/lib/event-participants"]
    return [string trim $result]
}

ad_proc im_event_resource_component { event_id form_mode plugin_id view_name orderby return_url } {
    Returns a formatted HTML showing the event resources
} {
    set params [list \
                    [list event_id $event_id] \
                    [list form_mode $form_mode] \
                    [list plugin_id $plugin_id] \
                    [list view_name $view_name] \
                    [list orderby $orderby] \
                    [list return_url $return_url] \
    ]

    set result [ad_parse_template -params $params "/packages/intranet-events/lib/event-resources"]
    return [string trim $result]
}


# ---------------------------------------------------------------------
# Events Permissions
# ---------------------------------------------------------------------

ad_proc -public im_event_permissions {user_id event_id view_var read_var write_var admin_var} {
    Fill the "by-reference" variables read, write and admin
    with the permissions of $user_id on $event_id
} {
    upvar $view_var view
    upvar $read_var read
    upvar $write_var write
    upvar $admin_var admin
    
    set view 0
    set read 0
    set write 0
    set admin 0
    
    set admin_p [im_is_user_site_wide_or_intranet_admin $user_id]
    set add_events_p [im_permission $user_id "add_events"]
    set view_events_all_p [im_permission $user_id "view_events_all"]
    set edit_events_all_p [im_permission $user_id "edit_events_all"]

    # Determine the list of all groups in which the current user is a member
    set user_parties [im_profile::profiles_for_user -user_id $user_id]
    lappend user_parties $user_id

    if {![db_0or1row event_info "
	select	coalesce(o.creation_user,0) as creation_user_id,
		(select count(*) from (
			-- member of the event - any role_id will do.
			select	distinct r.object_id_one
			from	acs_rels r,
				im_biz_object_members bom
			where	r.rel_id = bom.rel_id and
				r.object_id_two in ([join $user_parties ","])
		) t) as event_member_p,
		(select count(*) from (
			-- admin of the event
			select	distinct r.object_id_one
			from	acs_rels r,
				im_biz_object_members bom
			where	r.rel_id = bom.rel_id and
				r.object_id_two in ([join $user_parties ","]) and
				bom.object_role_id in (1301, 1302)
		) t) as event_admin_p,
		(select count(*) from (
			-- cases with user as task_assignee
			select distinct wfc.object_id
			from	wf_task_assignments wfta,
				wf_tasks wft,
				wf_cases wfc
			where	e.event_id = wfc.object_id and
				wft.state in ('enabled', 'started') and
				wft.case_id = wfc.case_id and
				wfta.task_id = wft.task_id and
				wfta.party_id in (
					select	group_id
					from	group_distinct_member_map
					where	member_id = :user_id
				    UNION
					select	:user_id
				)
		) t) as case_assignee_p,
		(select count(*) from (
			-- cases with user as task holding_user
			select	distinct wfc.object_id
			from	wf_tasks wft,
				wf_cases wfc
			where	e.event_id = wfc.object_id and
				wft.holding_user = :user_id and
				wft.state in ('enabled', 'started') and
				wft.case_id = wfc.case_id
		) t) as holding_user_p
	from	im_events e,
		acs_objects o
	where	e.event_id = :event_id and
		e.event_id = o.object_id
    "]} {
	# Didn't find event 
	if {$add_events_p} {
	    # The user has the right to create new events, so return 1 for all perms
	    set view 1
	    set read 1
	    set write 1
	    set admin 1
	}

	# Just return with permissions set to 0...
	return 0
    }

    set owner_p [expr $user_id == $creation_user_id]

    set read [expr $admin_p || $owner_p || $event_member_p || $holding_user_p || $case_assignee_p || $view_events_all_p]
    set write [expr $admin_p || $owner_p || $event_admin_p || $holding_user_p || $case_assignee_p || $edit_events_all_p]

    set view $read
    set admin $write

#     ad_return_complaint 1 "read = $read = expr $admin_p || $owner_p || $event_member_p || $holding_user_p || $case_assignee_p || $view_events_all_p<br>write = $write = expr $admin_p || $owner_p || $event_admin_p || $holding_user_p || $case_assignee_p || $edit_events_all_p"
}


# ----------------------------------------------------------------------
# Navigation Bar
# ---------------------------------------------------------------------

ad_proc -public im_event_navbar { 
    {-navbar_menu_label "events"}
    default_letter 
    base_url 
    next_page_url 
    prev_page_url 
    export_var_list 
    {select_label ""} 
} {
    Returns rendered HTML code for a horizontal sub-navigation
    bar for /intranet-events/.
    The lower part of the navbar also includes an Alpha bar.

    @param default_letter none marks a special behavious, hiding the alpha-bar.
    @navbar_menu_label Determines the "parent menu" for the menu tabs for 
       search shortcuts, defaults to "projects".
} {
    # -------- Defaults -----------------------------
    set user_id [ad_get_user_id]
    set url_stub [ns_urldecode [im_url_with_query]]

    set sel "<td class=tabsel>"
    set nosel "<td class=tabnotsel>"
    set a_white "<a class=whitelink"
    set tdsp "<td>&nbsp;</td>"

    # -------- Calculate Alpha Bar with Pass-Through params -------
    set bind_vars [ns_set create]
    foreach var $export_var_list {
	upvar 1 $var value
	if { [info exists value] } {
	    ns_set put $bind_vars $var $value
	}
    }
    set alpha_bar [im_alpha_bar -prev_page_url $prev_page_url -next_page_url $next_page_url $base_url $default_letter $bind_vars]
    set alpha_bar ""

    # Get the Subnavbar
    set parent_menu_sql "select menu_id from im_menus where label = '$navbar_menu_label'"
    set parent_menu_id [util_memoize [list db_string parent_admin_menu $parent_menu_sql -default 0]]
    
    ns_set put $bind_vars letter $default_letter
    ns_set delkey $bind_vars project_status_id

    set navbar [im_sub_navbar $parent_menu_id $bind_vars $alpha_bar "tabnotsel" $select_label]

    return $navbar
}


# ---------------------------------------------------------------------
# Event Cube
# ---------------------------------------------------------------------


ad_proc im_event_cube {
    {-param_hash_values "" }
    {-event_status_id "" }
    {-event_type_id "" }
    {-event_material_id "" }
    {-event_cost_center_id "" }
    {-event_location_id "" }
    {-event_creator_id "" }
    {-event_name "" }
    {-report_start_date "" }
    {-report_end_date ""}
    {-report_user_group_id "" }
    {-report_event_selection "" }
    {-report_user_selection "" }
    {-report_location_selection ""}
    {-report_resource_selection "" }
} {
    Returns a rendered cube with a graphical event display.
} {
    set user_url "/intranet/users/view"
    set location_url "/intranet-confdb/new"
    set resource_url "/intranet-confdb/new"
    set date_format "YYYY-MM-DD"
    set current_user_id [ad_get_user_id]
    set bgcolor(0) " class=roweven "
    set bgcolor(1) " class=rowodd "
    set name_order [parameter::get -package_id [apm_package_id_from_key intranet-core] -parameter "NameOrder" -default 1]
    set cell_width [parameter::get -package_id [apm_package_id_from_key intranet-events] -parameter "EventCubeCellWidth" -default 50]

    # Show left dimension entry (user, location or resource)
    # every few weeks in the Saturday + Sunday columns
    set show_kw_every_n_weeks 4

    if {-1 == $event_type_id} { set event_type_id "" }
    set report_start_date_julian [im_date_ansi_to_julian $report_start_date]
    set report_end_date_julian [im_date_ansi_to_julian $report_end_date]
    set report_days [expr $report_end_date_julian - $report_start_date_julian]

    # Deal with parameters from index.tcl page
    array set param_hash $param_hash_values
    set pass_through_vars [list]
    foreach var [array names param_hash] {
	lappend pass_through_vars $var
	set $var $param_hash($var)
    }
    

    # ---------------------------------------------------------------
    # Limit the number of users and days
    # ---------------------------------------------------------------

    set event_criteria [list]
    lappend event_criteria "e.event_start_date <= :report_end_date::date"
    lappend event_criteria "e.event_end_date >= :report_start_date::date"

    if {"" != $event_type_id && 0 != $event_type_id} {
	lappend event_criteria "e.event_type_id = '$event_type_id'"
    }
    if {"" != $event_status_id && 0 != $event_status_id} {
	lappend event_criteria "e.event_status_id = '$event_status_id'"
    }
    if {"" != $event_creator_id && 0 != $event_creator_id} {
	lappend event_criteria "o.creation_user = '$event_creator_id'"
    }
    if {"" != $event_material_id && 0 != $event_material_id} {
	lappend event_criteria "e.event_material_id = '$event_material_id'"
    }
    if {"" != $event_cost_center_id && 0 != $event_cost_center_id} {
	lappend event_criteria "e.event_cost_center_id = '$event_cost_center_id'"
    }
    if {"" != $event_location_id && 0 != $event_location_id} {
	lappend event_criteria "e.event_location_id = '$event_location_id'"
    }
    if {"" != $event_name} {
	set event_name_norm [db_string norm_event_name "select norm_text(:event_name)"]
	set event_name_ts [join $event_name_norm " & "]
	lappend event_criteria "e.event_id in (
		select	so.object_id
		from	im_search_objects so
		where	so.object_type_id = 10 and	-- im_event object type
			so.fti @@ to_tsquery(:event_name_ts)
	)"
    }

    # Limits on the number of events shown
    switch $report_event_selection {
	"" - all {
	}
	mine {
	    # Limit the list of _events_ to the ones related to the current user
	    lappend event_criteria "(
		e.event_id in (
			select object_id_two
			from acs_rels
			where object_id_one = :current_user_id
		UNION	select object_id
			from acs_objects
			where creation_user = :current_user_id
		)
	    )"
	}
	default  {
	    ad_return_complaint 1 "im_event_cube: Found unknown report_event_select=$report_event_selection"
	}
    }

    set event_where_clause [join $event_criteria " and\n            "]
    if {![empty_string_p $event_where_clause]} {
	set event_where_clause " and $event_where_clause"
    }

    # ---------------------------------------------------------------
    # Determine Top Dimension
    # ---------------------------------------------------------------
    
    # Initialize the hash for holidays.
    set bank_holiday_color [util_memoize [list db_string holiday_color "select aux_string2 from im_categories where category_id = [im_user_absence_type_bank_holiday]"]]
    set days_of_week_pretty "Sun Mon Tue Wed Thu Fri Sat"

    array set holiday_hash {}
    set day_list [list]
    for {set i 0} {$i < $report_days} {incr i} {
	db_1row date_info "
	    select 
		to_char(:report_start_date::date + :i::integer, :date_format) as date_date,
		to_char(:report_start_date::date + :i::integer, 'Day') as date_day,
		to_char(:report_start_date::date + :i::integer, 'dd') as date_day_of_month,
		to_char(:report_start_date::date + :i::integer, 'Mon') as date_month,
		to_char(:report_start_date::date + :i::integer, 'YYYY') as date_year,
		to_char(:report_start_date::date + :i::integer, 'Dy') as date_weekday,
		extract(dow FROM :report_start_date::date + :i::integer) AS date_day_of_week,
		extract(week FROM :report_start_date::date + :i::integer) AS date_week
        "

	set date_day_of_week_notrans [lindex $days_of_week_pretty $date_day_of_week]
	set date_day_of_week_pretty [lang::message::lookup "" intranet-events.Weekday_$date_day_of_week_notrans $date_day_of_week_notrans]
	if {$date_weekday == "Sat" || $date_weekday == "Sun"} { set holiday_hash($date_date) $bank_holiday_color }
	set date_month_l10n [lang::message::lookup "" intranet-events.Month_$date_month $date_month]
	lappend day_list [list $date_date $date_day_of_month $date_month_l10n $date_year $date_day_of_week_pretty $date_week $date_day_of_week]
    }


    # ---------------------------------------------------------------
    # Determine the User left dimension
    # ---------------------------------------------------------------

    set user_criteria [list]
    if {"" != $report_user_group_id && 0 != $report_user_group_id} {
	lappend user_criteria "u.user_id in (select member_id from group_distinct_member_map where group_id = :report_user_group_id)"
    }

    # Limit on the number of users shown
    switch $report_user_selection {
	"" - consultants_with_events {
	    lappend user_criteria "u.user_id in (select member_id from group_distinct_member_map where group_id = [im_profile_consultants])"
	    lappend user_criteria "u.user_id in (
			-- users who are member of any event
			select distinct
				u.user_id as user_id
			from	users u,
				acs_rels r,
				im_events e
			where	r.object_id_one = e.event_id and
				r.object_id_two = u.user_id
				$event_where_clause
	    )"
	}
	all_consultants {
	    lappend user_criteria "u.user_id in (select member_id from group_distinct_member_map where group_id = [im_profile_consultants] )"
	}
	employees_with_events {
	    lappend user_criteria "u.user_id in (select member_id from group_distinct_member_map where group_id = [im_profile_employees])"
	    lappend user_criteria "u.user_id in (
			-- users who are member of any event
			select distinct
				u.user_id as user_id
			from	users u,
				acs_rels r,
				im_events e
			where	r.object_id_one = e.event_id and
				r.object_id_two = u.user_id
				$event_where_clause
	    )"
	}
	all_employees {
	    lappend user_criteria "u.user_id in (select member_id from group_distinct_member_map where group_id = [im_profile_employees])"
	}
	none {
	    lappend user_criteria "u.user_id in (select 0 from dual)"
	}
	default  {
	    ad_return_complaint 1 "im_event_cube: Found unknown report_user_select=$report_user_selection"
	}
    }

    set user_where_clause [join $user_criteria " and\n            "]
    if {![empty_string_p $user_where_clause]} {
	set user_where_clause " and $user_where_clause"
    }

    # Select the list of users to be shown as left dimensions
    set user_list_sql "
		select	user_id,
			user_name,
			department,
			employee_office_id as office_id,
			(select office_path from im_offices where office_id = employee_office_id) as office_path
		from	(
			select distinct
				u.user_id as user_id,
				im_name_from_user_id(u.user_id, $name_order) as user_name,
				acs_object__name(emp.department_id) as department,
				(select	coalesce(min(o.office_id), 1)
					from	im_offices o,
						acs_rels r
					where	r.object_id_one = o.office_id and
						r.object_id_two = u.user_id
				) as employee_office_id
			from	users u
				LEFT OUTER JOIN im_employees emp ON u.user_id = emp.employee_id
			where	u.user_id not in (
					-- exclude deleted users
					select	u.user_id
					from	users u,
						acs_rels r,
						membership_rels mr
					where	r.rel_id = mr.rel_id and
						r.object_id_two = u.user_id and
						r.object_id_one = -2 and
						mr.member_state != 'approved'
				)
				$user_where_clause
			) t
		order by office_path, user_name
    "
    set user_list [db_list_of_lists user_list $user_list_sql]

    # ---------------------------------------------------------------
    # Determine the Location left dimension
    # ---------------------------------------------------------------

    set location_criteria [list]
    switch $report_location_selection {
	"" - locations_with_events {
	    lappend location_criteria "ci.conf_item_id in (
			select	e.event_location_id
			from	im_events e
			where	1 = 1
				$event_where_clause
	    )"
	}
	all_locations {
	    # Nothing - just the entire list of locations
	}
	none {
	    lappend location_criteria "ci.conf_item_id in (select 0 from dual)"
	}
	default  {
	    ad_return_complaint 1 "im_event_cube: Found unknown report_location_select=$report_location_selection"
	}
    }

    set location_where_clause [join $location_criteria " and\n            "]
    if {![empty_string_p $location_where_clause]} {
	set location_where_clause " and $location_where_clause"
    }

    set location_list [db_list_of_lists location_list "
		select distinct
			ci.conf_item_id as location_id,
			ci.conf_item_name as location_name,
			ci.conf_item_nr as location_nr,
			ci.room_number_seats as location_number_seats,
			coalesce(ci.description, '') || ' ' || coalesce(ci.note, '') as location_note
		from	im_conf_items ci
		where	ci.conf_item_name is not null and
			ci.conf_item_type_id in (81500) and	-- only Room CIs
			ci.conf_item_status_id not in ([im_conf_item_status_deleted])
			$location_where_clause
		order by ci.conf_item_name
    "]

    # ---------------------------------------------------------------
    # Determine the Resource left dimension
    # ---------------------------------------------------------------


    set resource_criteria [list]
    switch $report_resource_selection {
	"" - resources_with_events {
	    lappend resource_criteria "ci.conf_item_id in (
			select	r.object_id_two
			from	im_events e,
				acs_rels r
			where	r.object_id_one = e.event_id
				$event_where_clause
	    )"
	}
	all_resources {
	    # Nothing - just the entire list of resources
	}
	none {
	    lappend resource_criteria "ci.conf_item_id in (select 0 from dual)"
	}
	default  {
	    ad_return_complaint 1 "im_event_cube: Found unknown report_resource_select=$report_resource_selection"
	}
    }

    set resource_where_clause [join $resource_criteria " and\n            "]
    if {![empty_string_p $resource_where_clause]} {
	set resource_where_clause " and $resource_where_clause"
    }

    set resource_list [db_list_of_lists resource_list "
		select distinct
			ci.conf_item_id,
			ci.conf_item_name as resource_name,
			coalesce(ci.description, '') || ' ' || coalesce(ci.note, '') as resource_note
		from	im_conf_items ci
		where	ci.conf_item_status_id not in ([im_conf_item_status_deleted]) and
			ci.conf_item_type_id not in (81500)	-- exclude Room CIs
			$resource_where_clause
		order by ci.conf_item_name
     "]

    # ---------------------------------------------------------------
    # Events per user
    # ---------------------------------------------------------------
    
    set event_sql "
	select	e.*,
		acs_object__name(e.event_location_id) as event_location_name,
		(select conf_item_nr from im_conf_items where conf_item_id = e.event_location_id) as event_location_nr,
		u.user_id,
		e.event_start_date::date as start_d,
		to_char(e.event_start_date, 'J') as start_j,
		e.event_end_date::date as end_d,
		(e.event_end_date::date - e.event_start_date::date + 1) as event_duration,
		im_biz_object_member__list(e.event_id) as event_members,
		CASE WHEN e.event_start_date < :report_start_date THEN 1 ELSE 0 END as event_starts_before_report_p,
		cc.cost_center_label as event_cost_center_code,
		(select count(*) from acs_rels qr, im_biz_object_members qbom 
		 where qr.rel_id = qbom.rel_id and qbom.member_status_id = 82200 and
		 qr.object_id_one = e.event_id and
		 qr.object_id_two in (select member_id from group_distinct_member_map where group_id = 461)
		) as event_booked_seats,
		(select count(*) from acs_rels qr, im_biz_object_members qbom 
		 where qr.rel_id = qbom.rel_id and qbom.member_status_id = 82210 and
		 qr.object_id_one = e.event_id and
		 qr.object_id_two in (select member_id from group_distinct_member_map where group_id = 461)
		) as event_reserved_seats,
		coalesce(m.abbreviation, m.material_nr) as event_material_nr
	from	acs_objects o,
		im_events e
		LEFT OUTER JOIN im_cost_centers cc ON (e.event_cost_center_id = cc.cost_center_id)
		LEFT OUTER JOIN im_materials m ON (e.event_material_id = m.material_id)
		LEFT OUTER JOIN acs_rels r ON (r.object_id_one = e.event_id)
		LEFT OUTER JOIN users u ON (r.object_id_two = u.user_id)
	where	o.object_id = e.event_id
		$event_where_clause
    "
    array set user_event_hash {}
    array set location_event_hash {}
    db_foreach events $event_sql {

	# User Event Hash
	set key "$user_id-$start_d"
	set value ""
	if {[info exists user_event_hash($key)]} { set value $user_event_hash($key) }
	lappend value $event_id
	set user_event_hash($key) $value

	# Location Event Hash
	set key "$event_location_id-$start_d"
	set value ""
	if {[info exists location_event_hash($key)]} { set value $location_event_hash($key) }
	lappend value $event_id
	set location_event_hash($key) $value

	# Duplicate hash
	for {set event_j $start_j} {$event_j < [expr $start_j + $event_duration]} { incr event_j } {
	    set event_ansi [im_date_julian_to_ansi $event_j]

	    set events {}
	    set key "$user_id-$event_ansi"
	    if {[info exists collision_checker_hash($key)]} { set events $collision_checker_hash($key) }
	    lappend events $event_id
	    set collision_checker_hash($key) $events

	    set events {}
	    set key "$event_location_id-$event_ansi"
	    if {[info exists collision_checker_hash($key)]} { set events $collision_checker_hash($key) }
	    lappend events $event_id
	    set events [lsort -unique $events]
	    set collision_checker_hash($key) $events
	}

	# Split event members (just im_biz_object_member) into
	# customers and employees
	set event_members_pretty [list]
	set event_members_customers [list]
	set event_members_types [list]
	foreach tuple $event_members {
	    set member_id [lindex $tuple 0]
	    lappend event_members_pretty [util_memoize [list im_name_from_user_id $member_id] 3600]
	    lappend event_members_types [util_memoize [list db_string otype "select object_type from acs_objects where object_id = $member_id" -default ""] 3600]

	    set customer [util_memoize [list db_string cust "
		select	min(company_name)
		from	acs_rels r,
			im_companies c
		where	r.object_id_one = c.company_id and
			r.object_id_two = $member_id
	    " -default ""] 3600]
	    lappend event_members_customers $customer
	}

	set event_start_hash($key) $event_id
	set event_info_hash($event_id) [list \
					    event_id $event_id \
					    event_name $event_name \
					    event_nr $event_nr \
					    event_start_date $start_d \
					    event_start_date_julian $start_j \
					    event_end_date $end_d \
					    event_type_id $event_type_id \
					    event_status_id $event_status_id \
					    event_duration $event_duration \
					    event_location_nr $event_location_nr \
					    event_location_name $event_location_name \
					    event_members $event_members \
					    event_members_pretty $event_members_pretty \
					    event_members_customers $event_members_customers \
					    event_members_types $event_members_types \
					    event_material_nr $event_material_nr \
					    event_cost_center_code $event_cost_center_code \
					    event_booked_seats $event_booked_seats \
					    event_reserved_seats $event_reserved_seats \
					    event_consultant_abbreviation $event_consultant_abbreviation \
					    event_location_abbreviation $event_location_abbreviation \
					    event_resource_abbreviation $event_resource_abbreviation \
					   ]

	# Remember the events that starting before the report interval
	if {$event_starts_before_report_p} {
	    # Events by user
	    set events [list]
	    if {[info exists user_event_before_reporting_interval_hash($user_id)]} { 
		set events $user_event_before_reporting_interval_hash($user_id)
	    }
	    lappend events $event_id
	    set user_event_before_reporting_interval_hash($user_id) $events


	    # Events by location
	    set events [list]
	    if {[info exists location_event_before_reporting_interval_hash($event_location_id)]} { 
		set events $location_event_before_reporting_interval_hash($event_location_id)
	    }
	    lappend events $event_id
	    set location_event_before_reporting_interval_hash($event_location_id) $events

	}
    }

    # ---------------------------------------------------------------
    # Resources per user
    # ---------------------------------------------------------------
    set resource_sql "
	select	*,
		e.event_start_date::date as start_d,
		to_char(e.event_start_date, 'J') as start_j,
		CASE WHEN e.event_start_date < :report_start_date THEN 1 ELSE 0 END as resource_starts_before_report_p
	from	im_conf_items ci,
		acs_rels r,
		im_events e,
		acs_objects o
	where	o.object_id = e.event_id and
		r.object_id_one = e.event_id and
		r.object_id_two = ci.conf_item_id and
		e.event_start_date <= :report_end_date::date and
		e.event_end_date >= :report_start_date::date
		$event_where_clause
    "
    array set resource_hash {}
    db_foreach resources $resource_sql {

	# Resource Hash
	set key "$conf_item_id-$start_d"
	set value ""
	if {[info exists resource_hash($key)]} { set value $resource_hash($key) }
	lappend value $event_id
	set resource_hash($key) $value

	# Remember the resources that starting before the report interval
	if {$resource_starts_before_report_p} {
	    set events [list]
	    if {[info exists resource_before_reporting_interval_hash($conf_item_id)]} {
		set events $resource_before_reporting_interval_hash($conf_item_id)
	    }
	    lappend events $event_id
	    set resource_before_reporting_interval_hash($conf_item_id) $events
	}
    }

    # ---------------------------------------------------------------
    # Tasks per user
    # ---------------------------------------------------------------
    
    set task_sql "
	select	u.user_id,
		d.d,
		sum(coalesce(bom.percentage, 0.0)) as percentage
	from	im_projects p
		LEFT OUTER JOIN im_timesheet_tasks t ON (p.project_id = t.task_id),
		acs_rels r,
		im_biz_object_members bom,
		users u,
		(select im_day_enumerator as d from im_day_enumerator(:report_start_date, :report_end_date)) d
	where	
		r.object_id_one = p.project_id and
		r.object_id_two = u.user_id and
		r.rel_id = bom.rel_id and
		p.start_date <= :report_end_date::date and
		p.end_date >= :report_start_date::date and
                date_trunc('day',d.d) between date_trunc('day',p.start_date) and date_trunc('day',p.end_date)
		and not exists (select event_id from im_events where event_timesheet_task_id = p.project_id)
	group by u.user_id, d.d
    "
    array set task_hash {}
    db_foreach tasks $task_sql {
	set key "$user_id-$d"
	set value 0.0
	if {[info exists task_hash($key)]} { set value $task_hash($key) }
	set value [expr $value + $percentage]
	if {$value > 0.0} { set task_hash($key) $value }
    }

    # ---------------------------------------------------------------
    # Absences per user
    # ---------------------------------------------------------------

    array set absence_hash {}
    set absence_sql "
	-- Individual Absences per user
	select	a.absence_type_id,
		a.owner_id,
		d.d
	from	im_user_absences a,
		users u,
		(select im_day_enumerator as d from im_day_enumerator(:report_start_date, :report_end_date)) d
	where	a.owner_id = u.user_id and
		a.start_date <= :report_end_date::date and
		a.end_date >= :report_start_date::date and
                date_trunc('day',d.d) between date_trunc('day',a.start_date) and date_trunc('day',a.end_date)
     UNION
	-- Absences for user groups
	select	a.absence_type_id,
		mm.member_id as owner_id,
		d.d
	from	im_user_absences a,
		users u,
		group_distinct_member_map mm,
		(select im_day_enumerator as d from im_day_enumerator(:report_start_date, :report_end_date)) d
	where	mm.member_id = u.user_id and
		a.start_date <= :report_end_date::date and
		a.end_date >= :report_start_date::date and
                date_trunc('day',d.d) between date_trunc('day',a.start_date) and date_trunc('day',a.end_date) and 
		mm.group_id = a.group_id
    "

    # ToDo: re-factor so that color codes also work in case of more than 10 absence types
    db_foreach absences $absence_sql {
	set key "$owner_id-$d"
	set value ""
	if {[info exists absence_hash($key)]} { set value $absence_hash($key) }
	set absence_type_color [util_memoize [list db_string color "select aux_string2 from im_categories where category_id = $absence_type_id"]]
	lappend value $absence_type_color
	set absence_hash($key) $value
    }


    # ---------------------------------------------------------------
    # Conflict Checker
    # ---------------------------------------------------------------

    # Users
    foreach user_tuple $user_list {
	set user_id [lindex $user_tuple 0]
	set user_name [lindex $user_tuple 1]
	set user_dept [lindex $user_tuple 2]

	foreach day $day_list {
	    set date_date [lindex $day 0]

	    # Conflict Checker for Users
	    set key "$user_id-$date_date"
	    set absence_p [info exists absence_hash($key)]
	    set event_ids {}
	    if {[info exists collision_checker_hash($key)]} { set event_ids $collision_checker_hash($key) }
	    set percentage 0.0
	    if {[info exists task_hash($key)]} { set percentage $task_hash($key) }
	    set busy_p [expr $absence_p || $percentage > 0]

	    # ns_log Notice "conflict checker: key=$key, absence_p=$absence_p, event_ids=$event_ids, busy_p=$busy_p"

	    # Busy (absence or project assignment) + one event => conflict
	    if {$busy_p && [llength $event_ids] > 0} { 
		set user_event_key "$user_id-$event_ids"
		set conflict_hash($user_event_key) 1
	    }
	    # Two events => conflict
	    if {[llength $event_ids] > 1} { 
		foreach eid $event_ids {
		    set user_event_key "$user_id-$eid"
		    set conflict_hash($user_event_key) 1
		}

	    }
	}
    }

    # Locations
    foreach location_tuple $location_list {
	set location_id [lindex $location_tuple 0]
	set location_name [lindex $location_tuple 1]
	set location_nr [lindex $location_tuple 2]
	set location_seats [lindex $location_tuple 3]
	set location_note [lindex $location_tuple 4]

	foreach day $day_list {
	    set date_date [lindex $day 0]

	    # Conflict Checker for Locations
	    set key "$location_id-$date_date"
	    set absence_p [info exists absence_hash($key)]
	    set event_ids {}
	    if {[info exists collision_checker_hash($key)]} { set event_ids $collision_checker_hash($key) }
	    set percentage 0.0
	    if {[info exists task_hash($key)]} { set percentage $task_hash($key) }
	    set busy_p [expr $absence_p || $percentage > 0]

	    # Busy (absence or project assignment) + one event => conflict
	    if {$busy_p && [llength $event_ids] > 0} { 
		set location_event_key "$location_id-$event_ids"
		set conflict_hash($location_event_key) 1
	    }
	    # Two events => conflict
	    if {[llength $event_ids] > 1} { 
		foreach eid $event_ids {
		    set location_event_key "$location_id-$eid"
		    set conflict_hash($location_event_key) 1
		}

	    }
	}
    }


    # ---------------------------------------------------------------
    # Moving on time axis
    # ---------------------------------------------------------------

    set form_vars [ns_conn form]
    set export_vars_list [list]
    foreach form_var [ad_ns_set_keys $form_vars] {
	if {"start_date" == $form_var} { continue }
        set form_val [ns_set get $form_vars $form_var]
	lappend export_vars_list [list $form_var $form_val]
    }
      
    # Arrows to move time axis
    set arrow_days 7
    set arrow_double_days $report_days

    set arrow_left_report_start_date [db_string left_date "select :report_start_date::date - $arrow_days from dual"]
    set arrow_left_url [export_vars -base "/intranet-events/index" [linsert $export_vars_list end [list start_date $arrow_left_report_start_date]]]
    set arrow_left "<a href=$arrow_left_url>&lt;</a>"

    set arrow_left_double_report_start_date [db_string left_double_date "select :report_start_date::date - $arrow_double_days from dual"]
    set arrow_left_double_url [export_vars -base "/intranet-events/index" [linsert $export_vars_list end [list start_date $arrow_left_double_report_start_date]]]
    set arrow_left_double "<a href=$arrow_left_double_url>&lt;&lt;</a>"

    set arrow_right_report_start_date [db_string right_date "select :report_start_date::date + $arrow_days from dual"]
    set arrow_right_url [export_vars -base "/intranet-events/index" [linsert $export_vars_list end [list start_date $arrow_right_report_start_date]]]
    set arrow_right "<a href=$arrow_right_url>&gt;</a>"

    set arrow_right_double_report_start_date [db_string right_double_date "select :report_start_date::date + $arrow_double_days from dual"]
    set arrow_right_double_url [export_vars -base "/intranet-events/index" [linsert $export_vars_list end [list start_date $arrow_right_double_report_start_date]]]
    set arrow_right_double "<a href=$arrow_right_double_url>&gt;&gt;</a>"





    # ---------------------------------------------------------------
    # Users Table
    # ---------------------------------------------------------------
    
    set table_html "<table>\n"

    # Row week of year
    set table_header "<tr class=rowtitle>\n"
    append table_header "<td class=rowtitle colspan=2 align=right>$arrow_left_double &nbsp;&nbsp; $arrow_left </td>\n"
    set cell_span 0
    set cell_week -1
    set ctr 0
    foreach day $day_list {
	set date_date [lindex $day 0]
	set date_day_of_month [lindex $day 1]
	set date_month_of_year [lindex $day 2]
	set date_year [lindex $day 3]
	set date_weekday [lindex $day 4]
	set date_week [lindex $day 5]
	set date_day_of_week [lindex $day 6]

	if {0 == $ctr} { set cell_week $date_week }
	if {$date_week != $cell_week} {
	    append table_header "<td class=rowtitle colspan=$cell_span align=center>[lang::message::lookup "" intranet-events.KW_week "%date_year% KW %cell_week%"]</td>\n"
	    set cell_span 1
	    set cell_week $date_week
	} else {
	    incr cell_span
	}
	incr ctr
    }
    append table_header "<td class=rowtitle colspan=$cell_span align=center>[lang::message::lookup "" intranet-events.KW_week "%date_year% KW %cell_week%"]</td>\n"
    append table_header "<td class=rowtitle>$arrow_right &nbsp;&nbsp; $arrow_right_double</td>\n"
    append table_header "</tr>\n"
    append table_html $table_header

    # A form with select box for filtering the users shown
    set user_pairs [list \
			consultants_with_events [lang::message::lookup "" intranet-events.Consultants_with_events "Consultants with events"] \
			all_consultants [lang::message::lookup "" intranet-events.All_consultants "All Consultants"] \
			employees_with_events [lang::message::lookup "" intranet-events.Employees_with_events "Employees with events"] \
			all_employees [lang::message::lookup "" intranet-events.All_employees "All Employees"] \
			none [lang::message::lookup "" intranet-events.No_users "No Users"] \
		       ]
    set user_select [im_select -javascript "onchange=\"this.form.submit()\"" -ad_form_option_list_style_p 0 -package_key "intranet-events" report_user_selection $user_pairs ""]
    # Restore the original parameters from the index.tcl query
    foreach var $pass_through_vars { set $var $param_hash($var) }
    set user_pass_through [export_vars -form -exclude report_user_selection $pass_through_vars]
    set user_form "<form name=report_user_selection action=/intranet-events/index> $user_select $user_pass_through</form>"


    # Row with month/day of month
    set table_header "<tr class=rowtitle>\n"
    append table_header "<td class=rowtitle colspan=2>$user_form</td>\n"
    foreach day $day_list {
	set date_date [lindex $day 0]
	set date_day_of_month [lindex $day 1]
	set date_month_of_year [lindex $day 2]
	set date_year [lindex $day 3]
	set date_weekday [lindex $day 4]
	set date_week [lindex $day 5]
	set date_day_of_week [lindex $day 6]
	append table_header "<td class=rowtitle align=center><div style=\"width: ${cell_width}px\">$date_weekday $date_day_of_month $date_month_of_year</div></td>\n"
    }
    append table_header "<td class=rowtitle></td>\n"
    append table_header "</tr>\n"
    append table_html $table_header


    # Row for each user
    set row_ctr 0
    set table_body ""
    foreach user_tuple $user_list {
	append table_body "<tr $bgcolor([expr $row_ctr % 2])>\n"
	set user_id [lindex $user_tuple 0]
	set user_name [lindex $user_tuple 1]
	set user_dept [lindex $user_tuple 2]
	set user_office_id [lindex $user_tuple 3]
	set user_office_name [lindex $user_tuple 4]

	set user_color_code "white"
	if {[im_column_exists im_offices solidline_color_code]} {
	    set user_color_code [util_memoize [list db_string office_color_code "select solidline_color_code from im_offices where office_id = '$user_office_id'" -default "lightcyan"]]
	}

	set office_url [export_vars -base "/intranet/offices/view" {return_url {office_id $user_office_id}}]
	append table_body "<td bgcolor='$user_color_code' colspan=2><nobr><a href='[export_vars -base $user_url {user_id}]'>$user_name</a></nobr><br><nobr><a href='$office_url'></a>$user_office_name</nobr></td>\n"

	# ------------------------------------------------------------
	# Deal with the events starting before the actual reporting interval
	set line_events [list]
	set events [list]
	if {[info exists user_event_before_reporting_interval_hash($user_id)]} {
	    set events $user_event_before_reporting_interval_hash($user_id)
	}
	set before_events_html ""
	foreach eid $events {
	    set event_values $event_info_hash($eid)
	    set conflict_key "$user_id-$eid"
	    set conflict_p [info exists conflict_hash($conflict_key)]
	    append before_events_html [im_event_cube_render_event \
					   -event_values $event_values \
					   -report_start_date_julian $report_start_date_julian \
					   -conflict_p $conflict_p \
					   -location user_list \
            ]
	    lappend line_events $eid
	}

	# ------------------------------------------------------------
	# Loop through the days for the user
	set week_ctr 1
	foreach day $day_list {
	    set date_date [lindex $day 0]
	    set date_day_of_month [lindex $day 1]
	    set date_month_of_year [lindex $day 2]
	    set date_year [lindex $day 3]
	    set date_weekday [lindex $day 4]
	    set date_week [lindex $day 5]
	    set date_day_of_week [lindex $day 6]

	    set key "$user_id-$date_date"
	    set value [list]
	    if {[info exists absence_hash($key)]} { set value [concat $value $absence_hash($key)] }
	    if {[info exists holiday_hash($date_date)]} { set value [concat $value $holiday_hash($date_date)] }

	    set percentage 0.0
	    if {[info exists task_hash($key)]} { set percentage $task_hash($key) }
	    if {$percentage > 0.0} { 
		set color [im_event_color_for_assignation -percentage $percentage]
		if {"" != $color} { set value [concat $value $color] }
	    }

	    # Determine if there is an event to show
	    set event_html ""
	    append event_html $before_events_html
	    set before_events_html ""
	    if {[info exists user_event_hash($key)]} { 
		set events $user_event_hash($key)
		foreach eid $events {
		    set event_values $event_info_hash($eid)
		    set conflict_key "$user_id-$eid"
		    set conflict_p [info exists conflict_hash($conflict_key)]
		    append event_html [im_event_cube_render_event \
					   -event_values $event_values \
					   -conflict_p $conflict_p \
					   -location user_list \
		    ]
		    lappend line_events $eid
		}
	    }
	    
	    # Shows date and consultant every N weeks. Add the user name 
	    # every Nth Saturday and the date + KW every Nth sunday
	    if {0 == $date_day_of_week} { incr week_ctr }
	    if {"" == $event_html} {
		if {[expr $week_ctr % $show_kw_every_n_weeks] == 1} {
		    if {0 == $date_day_of_week} {
			# Sunday - Add the date
			append event_html "KW $date_week<br>$date_day_of_month. $date_month_of_year"
		    }
		}
		
		if {[expr $week_ctr % $show_kw_every_n_weeks] == 0} {
		    if {6 == $date_day_of_week} { 
			# Saturday - Add the sconsultant
			append event_html "<nobr><a href='[export_vars -base $user_url {user_id}]'>$user_name</a></nobr><br><nobr><a href='$office_url'></a>$user_office_name"
		    }
		}
	    }

	    append table_body [im_event_cube_render_cell -value $value -event_html $event_html]
	    ns_log NOTICE "intranet-events-procs::im_event_cube_render_cell: $value"
	}

	# Show the user's name at the end of the line
	append table_body "<td bgcolor='$user_color_code' colspan=2><nobr><a href='[export_vars -base $user_url {user_id}]'>$user_name</a></nobr><br><nobr><a href='$office_url'></a>$user_office_name</nobr></td>\n"
	append table_body "</tr>\n"
	incr row_ctr
    }
    append table_html $table_body


    # ---------------------------------------------------------------
    # Locations
    # ---------------------------------------------------------------

    # A form with select box for filtering the locations shown
    set location_pairs [list \
			locations_with_events [lang::message::lookup "" intranet-events.Locations_with_events "Locations with events"] \
			all_locations [lang::message::lookup "" intranet-events.All_locations "All Locations"] \
			none [lang::message::lookup "" intranet-events.No_locations "No Locations"] \
		       ]
    set location_select [im_select -javascript "onchange=\"this.form.submit()\"" -ad_form_option_list_style_p 0 -package_key "intranet-events" report_location_selection $location_pairs ""]
    foreach var $pass_through_vars { set $var $param_hash($var) }
    set location_pass_through [export_vars -form -exclude report_location_selection $pass_through_vars]
    set location_form "<form name=report_location_selection action=/intranet-events/index> $location_select $location_pass_through</form>"

    # Row week of year
    set table_header "<tr class=rowtitle>\n"
    append table_header "<td class=rowtitle colspan=2 align=right>$arrow_left_double &nbsp;&nbsp; $arrow_left </td>\n"
    set cell_span 0
    set cell_week -1
    set ctr 0
    foreach day $day_list {
	set date_date [lindex $day 0]
	set date_day_of_month [lindex $day 1]
	set date_month_of_year [lindex $day 2]
	set date_year [lindex $day 3]
	set date_weekday [lindex $day 4]
	set date_week [lindex $day 5]
	set date_day_of_week [lindex $day 6]

	if {0 == $ctr} { set cell_week $date_week }
	if {$date_week != $cell_week} {
	    append table_header "<td class=rowtitle colspan=$cell_span align=center>[lang::message::lookup "" intranet-events.KW_week "%date_year% KW %cell_week%"]</td>\n"
	    set cell_span 1
	    set cell_week $date_week
	} else {
	    incr cell_span
	}
	incr ctr
    }
    append table_header "<td class=rowtitle colspan=$cell_span align=center>[lang::message::lookup "" intranet-events.KW_week "%date_year% KW %cell_week%"]</td>\n"
    append table_header "<td class=rowtitle>$arrow_right &nbsp;&nbsp; $arrow_right_double</td>\n"
    append table_header "</tr>\n"
    append table_html $table_header


    # Row Date
    set table_header "<tr class=rowtitle>\n"
    append table_header "<td class=rowtitle colspan=2>$location_form</td>\n"
    foreach day $day_list {
	set date_date [lindex $day 0]
	set date_day_of_month [lindex $day 1]
	set date_month_of_year [lindex $day 2]
	set date_year [lindex $day 3]
	set date_weekday [lindex $day 4]
	set date_week [lindex $day 5]
	set date_day_of_week [lindex $day 6]
	# append table_header "<td class=rowtitle>$date_month_of_year<br>$date_day_of_month</td>\n"
	append table_header "<td class=rowtitle align=center><div style=\"width: ${cell_width}px\">$date_weekday $date_day_of_month $date_month_of_year</div></td>\n"
    }
    append table_header "<td class=rowtitle></td>\n"
    append table_header "</tr>\n"
    append table_html $table_header


    # Rows for each location
    set row_ctr 0
    set table_body ""
    foreach location_tuple $location_list {
	append table_body "<tr $bgcolor([expr $row_ctr % 2])>\n"
	set location_id [lindex $location_tuple 0]
	set location_name [lindex $location_tuple 1]
	set location_nr [lindex $location_tuple 2]
	set location_seats [lindex $location_tuple 3]
	set location_note [lindex $location_tuple 4]
	append table_body "<td colspan=2><nobr><a href='[export_vars -base $location_url {{conf_item_id $location_id} {form_mode display}}]' title='${location_name}\n${location_note}'>$location_nr ($location_seats)</a><br>&nbsp;</nobr></td>\n"

	# Deal with the events starting before the actual reporting interval
	set events [list]
	if {[info exists location_event_before_reporting_interval_hash($location_id)]} {
	    set events $location_event_before_reporting_interval_hash($location_id)
	}
	set before_events_html ""
	foreach eid $events {
	    set event_values $event_info_hash($eid)

	    set conflict_key "$location_id-$eid"
	    set conflict_p [info exists conflict_hash($conflict_key)]
	    append before_events_html [im_event_cube_render_event \
					   -event_values $event_values \
					   -report_start_date_julian $report_start_date_julian \
					   -conflict_p $conflict_p \
					   -location location_list \
            ]
	}

	set week_ctr 1
	foreach day $day_list {
	    set date_date [lindex $day 0]
	    set date_day_of_month [lindex $day 1]
	    set date_month_of_year [lindex $day 2]
	    set date_year [lindex $day 3]
	    set date_weekday [lindex $day 4]
	    set date_week [lindex $day 5]
	    set date_day_of_week [lindex $day 6]

	    set key "$location_id-$date_date"
	    set value ""
	    if {[info exists holiday_hash($date_date)]} { append value $holiday_hash($date_date) }

	    set event_html ""
	    append event_html $before_events_html
	    set before_events_html ""


	    if {[info exists location_event_hash($key)]} { 
		set events $location_event_hash($key)
		foreach eid $events {
		    set event_values $event_info_hash($eid)
		    set conflict_key "$location_id-$eid"
		    set conflict_p [info exists conflict_hash($conflict_key)]
		    append event_html [im_event_cube_render_event \
					   -event_values $event_values \
					   -conflict_p $conflict_p \
					   -location location_list \
                    ]
		}
	    }



	    # Shows date and consultant every N weeks. Add the user name 
	    # every Nth Saturday and the date + KW every Nth sunday
	    if {0 == $date_day_of_week} { incr week_ctr }
	    if {"" == $event_html} {
		if {[expr $week_ctr % $show_kw_every_n_weeks] == 1} {
		    if {0 == $date_day_of_week} {
			# Sunday - Add the date
			append event_html "KW $date_week<br>$date_day_of_month. $date_month_of_year"		    
		    }
		}
		if {[expr $week_ctr % $show_kw_every_n_weeks] == 0} {
		    if {6 == $date_day_of_week} { 
			# Saturday - Add the sconsultant
			append event_html "<nobr><a href='[export_vars -base $location_url {location_id}]'>$location_nr ($location_seats)</a></nobr>"
		    }
		}
	    }


	    
	    append table_body [im_event_cube_render_cell -value $value -event_html $event_html]
	}
	append table_body "<td colspan=2><nobr><a href='[export_vars -base $location_url {{conf_item_id $location_id} {form_mode display}}]' title='${location_name}\n${location_note}'>$location_nr ($location_seats)</a></nobr></td>\n"
	append table_body "</tr>\n"
	incr row_ctr
    }
    append table_html $table_body



    # ---------------------------------------------------------------
    # Resources
    # ---------------------------------------------------------------

    # A form with select box for filtering the resources shown
    set resource_pairs [list \
			resources_with_events [lang::message::lookup "" intranet-events.Resources_with_events "Resources with events"] \
			all_resources [lang::message::lookup "" intranet-events.All_resources "All Resources"] \
			none [lang::message::lookup "" intranet-events.No_resources "No Resources"] \
		       ]
    set resource_select [im_select -javascript "onchange=\"this.form.submit()\"" -ad_form_option_list_style_p 0 -package_key "intranet-events" report_resource_selection $resource_pairs ""]
    foreach var $pass_through_vars { set $var $param_hash($var) }
    set resource_pass_through [export_vars -form -exclude report_resource_selection $pass_through_vars]
    set resource_form "<form name=report_resource_selection action=/intranet-events/index> $resource_select $resource_pass_through</form>"


    # Row week of year
    set table_header "<tr class=rowtitle>\n"
    append table_header "<td class=rowtitle colspan=2 align=right>$arrow_left_double &nbsp;&nbsp; $arrow_left </td>\n"
    set cell_span 0
    set cell_week -1
    set ctr 0
    foreach day $day_list {
	set date_date [lindex $day 0]
	set date_day_of_month [lindex $day 1]
	set date_month_of_year [lindex $day 2]
	set date_year [lindex $day 3]
	set date_weekday [lindex $day 4]
	set date_week [lindex $day 5]
	set date_day_of_week [lindex $day 6]

	if {0 == $ctr} { set cell_week $date_week }
	if {$date_week != $cell_week} {
	    append table_header "<td class=rowtitle colspan=$cell_span align=center>[lang::message::lookup "" intranet-events.KW_week "%date_year% KW %cell_week%"]</td>\n"
	    set cell_span 1
	    set cell_week $date_week
	} else {
	    incr cell_span
	}
	incr ctr
    }
    append table_header "<td class=rowtitle colspan=$cell_span align=center>[lang::message::lookup "" intranet-events.KW_week "%date_year% KW %cell_week%"]</td>\n"
    append table_header "<td class=rowtitle>$arrow_right &nbsp;&nbsp; $arrow_right_double</td>\n"
    append table_header "</tr>\n"
    append table_html $table_header

    # Row Date
    set table_header "<tr class=rowtitle>\n"
    append table_header "<td class=rowtitle colspan=2>$resource_form</td>\n"
    foreach day $day_list {
	set date_date [lindex $day 0]
	set date_day_of_month [lindex $day 1]
	set date_month_of_year [lindex $day 2]
	set date_year [lindex $day 3]
	set date_weekday [lindex $day 4]
	set date_week [lindex $day 5]
	set date_day_of_week [lindex $day 6]
	# append table_header "<td class=rowtitle>$date_month_of_year<br>$date_day_of_month</td>\n"
	append table_header "<td class=rowtitle align=center><div style=\"width: ${cell_width}px\">$date_weekday $date_day_of_month $date_month_of_year</div></td>\n"
    }
    append table_header "<td class=rowtitle></td>\n"
    append table_header "</tr>\n"
    append table_html $table_header

    
    set row_ctr 0
    set table_body ""
    foreach resource_tuple $resource_list {
	append table_body "<tr $bgcolor([expr $row_ctr % 2])>\n"
	set resource_id [lindex $resource_tuple 0]
	set resource_name [lindex $resource_tuple 1]
	set resource_note [lindex $resource_tuple 2]
	append table_body "<td colspan=2><nobr><a href='[export_vars -base $resource_url {{conf_item_id $resource_id} {form_mode display}}]' title='$resource_note'>$resource_name</a></nobr></td>\n"

	# Deal with the events starting before the actual reporting interval
	set events [list]
	if {[info exists resource_before_reporting_interval_hash($resource_id)]} {
	    set events $resource_before_reporting_interval_hash($resource_id)
	}
	set before_events_html ""
	foreach eid $events {
	    if {[catch {
		set event_values $event_info_hash($eid)
		set conflict_key "$resource_id-$eid"
		set conflict_p [info exists conflict_hash($conflict_key)]
		append before_events_html [im_event_cube_render_event \
					       -event_values $event_values \
					       -report_start_date_julian $report_start_date_julian \
					       -conflict_p $conflict_p \
					       -location resource_list \
                ]
	    } err_msg]} {
		append before_events_html "<div>$err_msg</div>"
	    }
	}

	set week_ctr 1
	foreach day $day_list {
	    set date_date [lindex $day 0]
	    set date_day_of_month [lindex $day 1]
	    set date_month_of_year [lindex $day 2]
	    set date_year [lindex $day 3]
	    set date_weekday [lindex $day 4]
	    set date_week [lindex $day 5]
	    set date_day_of_week [lindex $day 6]

	    set key "$resource_id-$date_date"
	    set value ""
            if {[info exists holiday_hash($date_date)]} { set value [concat $value $holiday_hash($date_date)] }

	    set event_html ""
	    append event_html $before_events_html
	    set before_events_html ""

	    if {[info exists resource_hash($key)]} { 
		set events $resource_hash($key)
		foreach eid $events {
		    set event_values $event_info_hash($eid)
		    set conflict_key "$resource_id-$eid"
		    set conflict_p [info exists conflict_hash($conflict_key)]
		    append event_html [im_event_cube_render_event \
					   -event_values $event_values \
					   -conflict_p $conflict_p \
					   -location resource_list \
                    ]
		}
	    }

	    # Shows date and consultant every N weeks. Add the user name 
	    # every Nth Saturday and the date + KW every Nth sunday
	    if {0 == $date_day_of_week} { incr week_ctr }
	    if {"" == $event_html} {
		if {[expr $week_ctr % $show_kw_every_n_weeks] == 1} {
		    if {0 == $date_day_of_week} {
			# Sunday - Add the date
			append event_html "KW $date_week<br>$date_day_of_month. $date_month_of_year"
		    }
		}
		if {[expr $week_ctr % $show_kw_every_n_weeks] == 0} {
		    if {6 == $date_day_of_week} { 
			# Saturday - Add the sconsultant
			append event_html "<nobr><a href='[export_vars -base $resource_url {{conf_item_id $resource_id} {form_mode display}}]' title='$resource_note'>$resource_name</a></nobr></td>\n"
		    }
		}
	    }
	    
	    append table_body [im_event_cube_render_cell -value $value -event_html $event_html]
	}
	append table_body "<td colspan=2><nobr><a href='[export_vars -base $resource_url {{conf_item_id $resource_id} {form_mode display}}]' title='$resource_note'>$resource_name</a></nobr></td>\n"
	append table_body "</tr>\n"
	incr row_ctr
    }
    append table_html $table_body

    append table_html "</table>\n"
}


ad_proc im_event_cube_render_event { 
    {-report_start_date_julian ""}
    {-conflict_p 0}
    {-location ""}
    -event_values:required
} {
    Renders a single event as HTML DIV on top of a table.
    The HTML needs to be inserted into the table cell 
    representing the day when the event starts.
} {
    # event_id, event_name, event_nr, event_type_id, event_status_id, event_duration
    # event_members, event_members_pretty
    array set event_local_info $event_values
    set event_id $event_local_info(event_id)
    set event_name $event_local_info(event_name)
    set event_nr $event_local_info(event_nr)
    set event_start_date $event_local_info(event_start_date)
    set event_start_date_julian $event_local_info(event_start_date_julian)
    set event_end_date $event_local_info(event_end_date)
    set event_status_id $event_local_info(event_status_id)
    set event_duration $event_local_info(event_duration)
    set event_location_nr $event_local_info(event_location_nr)
    set event_location_name $event_local_info(event_location_name)
    set event_members $event_local_info(event_members)
    set event_members_pretty $event_local_info(event_members_pretty)
    set event_members_customers $event_local_info(event_members_customers)
    set event_members_types $event_local_info(event_members_types)
    set event_material_nr $event_local_info(event_material_nr)
    set event_cost_center_code $event_local_info(event_cost_center_code)
    set event_booked_seats $event_local_info(event_booked_seats)
    set event_reserved_seats $event_local_info(event_reserved_seats)
    set event_consultant_abbreviation $event_local_info(event_consultant_abbreviation)
    set event_location_abbreviation $event_local_info(event_location_abbreviation)
    set event_resource_abbreviation $event_local_info(event_resource_abbreviation)

    set event_url [export_vars -base "/intranet-events/new" {{form_mode display} event_id}]

    set cell_width [parameter::get -package_id [apm_package_id_from_key intranet-events] -parameter "EventCubeCellWidth" -default 50]

    set consultants [list]
    set customers [list]
    set trainers [list]
    for {set i 0} {$i < [llength $event_members]} {incr i} {
	set rel_tuple_id [lindex $event_members $i]
	set user_id [lindex $rel_tuple_id 0]
	set role_id [lindex $rel_tuple_id 1]
	set user_name [lindex $event_members_pretty $i]
	set customer [lindex $event_members_customers $i]
	set otype [lindex $event_members_types $i]

	switch $otype {
	    user - person {
		if {[im_profile::member_p -profile_id [im_profile_employees] -user_id $user_id]} {
		    switch $role_id {
			1308 {
			    # Trainer role - special treatment
			    lappend trainers $user_name
			}
		    }
		    lappend consultants "$user_name"
		} else {
		    lappend customers "$customer - $user_name"
		}
	    }
	    im_conf_item {
		# These are the laptops and other resources associated with the event
		# Ignore at the moment
	    }
	    default {
		ad_return_complaint 1 "im_event_cube_render_event: unknown object_type: $otype"
	    }
	}
    }

    # Calculate the event code
    set kuerzel ""
    switch $location {
	"user_list" {
	    set kuerzel "$event_location_nr; $event_booked_seats; <i>$event_reserved_seats</i><br>$event_material_nr"
	    if {"" != $event_consultant_abbreviation} { set kuerzel $event_consultant_abbreviation }
	}
	"location_list" {
	    set event_consultant_list [list]
	    foreach p $trainers {
		set initials ""
		foreach n $p { append initials [string range $n 0 0] }
		lappend event_consultant_list $initials
	    }

	    set kuerzel "[join $event_consultant_list ";"]; $event_booked_seats; <i>$event_reserved_seats</i><br>$event_material_nr"
	    if {"" != $event_location_abbreviation} { set kuerzel $event_location_abbreviation }
	}
	"resource_list" {
	    set kuerzel "$event_location_nr"
	    if {"" != $event_resource_abbreviation} { set kuerzel $event_resource_abbreviation }
	}
	default {
	    ad_return_complaint 1 "im_event_cube_render_event: Unknown location: '$location'"
	}

    }

    # Deal with "broken" events, that start before the first 
    # column of the report
    set event_width_days $event_duration
    if {"" != $report_start_date_julian} {
	set event_width_days [expr $event_duration - ($report_start_date_julian - $event_start_date_julian)]
    }

    # Width: Multiples of the cell width
    set event_width [expr $event_width_days * [expr $cell_width + 6]]

    # Determine the color of the event
    set bgcolor [util_memoize [list db_string bgcolor "select aux_string2 from im_categories where category_id = '$event_status_id'" -default ""]]
    if {"" == $bgcolor} { set bgcolor "FFFFFF" }

    # What to show on a mouse-over
    set event_title "[lang::message::lookup "" intranet-events.Name Name]: $event_name
[lang::message::lookup "" intranet-events.Nr "Nr."]: $event_nr
[lang::message::lookup "" intranet-events.Location Location]: $event_location_nr - $event_location_name
[lang::message::lookup "" intranet-events.Material Material]: $event_material_nr
[lang::message::lookup "" intranet-events.Start Start]: $event_start_date
[lang::message::lookup "" intranet-events.End End]: $event_end_date
[lang::message::lookup "" intranet-events.Cost_Center "Cost Center"]: $event_cost_center_code
[lang::message::lookup "" intranet-events.Consultants Consultants]:
[join $consultants "\n"]
[lang::message::lookup "" intranet-events.Customers Customers]:
[join $customers "\n"]
"

    if {$conflict_p} { 
 	set bordercolor "red" 
        set border_width 2
        set top_distance -16
    } else {
        set bordercolor "yellow"
        set border_width 0
        set top_distance -13
    }
    set result "
      <div style='position: relative'>
<div style='position: absolute; top: $top_distance; left: -3; width: $event_width; z-index:10; background: yellow; opacity: 0.8;'>
<table cellspacing=0 cellpadding=0 border=$border_width bgcolor=#$bgcolor bordercolor=$bordercolor width='100%'>
<tr>
<td bgcolor=#$bgcolor>
<nobr><a href=$event_url title='$event_title' target='_blank'>$kuerzel</a>&nbsp;</nobr>
</td>
</tr>
</table>
</div>
</div>
"
    return $result
}


ad_proc im_event_cube_render_cell { 
    -value:required
    { -event_html "&nbsp;" }
} {
    Renders a single report cell, depending on value.
    Takes the color from events color lookup.
} {
    if {[catch {
	set color [im_util_mix_colors $value]
    } err_msg]} {
	ad_return_complaint 1 "im_absence_mix_colors $value<br><pre>$err_msg</pre>"
    }   

    if {"" != $color} {
        return "<td bgcolor=\#$color>$event_html</td>\n"
    } else {
        return "<td>$event_html</td>\n"
    }
}


ad_proc im_event_color_for_assignation { 
    -percentage:required
} {
    Returns a color representing 0% - 100% assignation
    of a user to a project
} {
    # Bad integer
    if {![string is double $percentage]} { 
	return "FF0000" 
    }
    if {$percentage < 0} { set percentage 0.0 }
    if {$percentage > 100} { 
	#mharms: return orange when percentage > 100
	return "FF896C"
    }
        
    set hex_list {0 1 2 3 4 5 6 7 8 9 A B C D E F}
    
    #mharms: base color (represents 1%)
    set r 245
    set g 245          
    set b 255
    
    #mharms: changed calculation
    set r [expr  $r - int($r * ($percentage / 100.0))]
    set g [expr  $g - int($g * ($percentage / 100.0))]
    set b 255
    
    set color ""
    append color [lindex $hex_list [expr $r / 16]]
    append color [lindex $hex_list [expr $r % 16]]
    append color [lindex $hex_list [expr $g / 16]]
    append color [lindex $hex_list [expr $g % 16]]
    append color [lindex $hex_list [expr $b / 16]]
    append color [lindex $hex_list [expr $b % 16]]
    
    return $color
}




ad_proc im_util_mix_colors {
    colors
} {
    Mixes a number of colors.
    Colors are expected in HEX format like "FFCC99".
    @param colors contains a list of colors to be mixed
} {
    # Show empty cells according to even/odd row formatting
    if {"" == $colors} { return "" }
    if {[string is integer $colors] && [expr $colors < 0]} { return "red" }
    set colors [string toupper $colors]

    set hex_list {0 1 2 3 4 5 6 7 8 9 A B C D E F}
    set len [llength $colors]
    set r 0
    set g 0
    set b 0
    
    # Mix the colors for each of the characters in "colors"
    foreach col $colors {
	set r [expr $r + [lsearch $hex_list [string range $col 0 0]] * 16]
	set r [expr $r + [lsearch $hex_list [string range $col 1 1]]]
	
	set g [expr $g + [lsearch $hex_list [string range $col 2 2]] * 16]
	set g [expr $g + [lsearch $hex_list [string range $col 3 3]]]
	
	set b [expr $b + [lsearch $hex_list [string range $col 4 4]] * 16]
	set b [expr $b + [lsearch $hex_list [string range $col 5 5]]]
    }
    
    # Calculate the median
    set r [expr $r / $len]
    set g [expr $g / $len]
    set b [expr $b / $len]

    # Convert the RGB values back into a hex color string
    set color ""
    append color [lindex $hex_list [expr $r / 16]]
    append color [lindex $hex_list [expr $r % 16]]
    append color [lindex $hex_list [expr $g / 16]]
    append color [lindex $hex_list [expr $g % 16]]
    append color [lindex $hex_list [expr $b / 16]]
    append color [lindex $hex_list [expr $b % 16]]

    return $color
}




ad_proc im_event_cube_color_list { } {
    Returns the list of colors for the various types of events
} {
#    return [util_memoize im_event_cube_color_list_helper]
    return [im_event_cube_color_list_helper]
}


ad_proc im_event_cube_color_list_helper { } {
    Returns the list of colors for the various types of events
} {
    # define default color set 
    set color_list {
        EC9559
        E2849B
        53A7D8
        A185CB
        FFF956
        CCCCC9
        CCCCC9
        CCCCC9
        CCCCC9
        CCCCC9
        CCCCC9
        CCCCC9
        CCCCC9
        CCCCC9
        CCCCC9
        CCCCC9
        CCCCC9
        CCCCC9
        CCCCC9
        CCCCC9
    }

    # Overwrite in case there's a custom color defined 
    set col_sql "
        select  category_id, category, enabled_p, aux_string2
        from    im_categories
        where	category_type = 'Intranet Event Type'
        order by category_id
     "

    set ctr 0 
    db_foreach cols $col_sql {
	if { "" == $aux_string2 } {
	    lset color_list $ctr [lindex $color_list $ctr]
	} else {
	    lset color_list $ctr $aux_string2
	}
	incr ctr
    }
    return $color_list

}
