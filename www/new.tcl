# /packages/intranet-events/www/new.tcl
#
# Copyright (c) 2003-2013 ]project-open[
#
# All rights reserved. Please check
# http://www.project-open.com/license/ for details.


# -----------------------------------------------------------
# Page Head
#
# There are two different heads, depending whether it's called
# "standalone" (TCL-page) or as a Workflow Panel.
# -----------------------------------------------------------

# Skip if this page is called as part of a Workflow panel
if {![info exists task]} {

    ad_page_contract {
	@author frank.bergmann@project-open.com
    } {
	event_id:integer,optional
	{ event_name "" }
	{ event_nr "[im_event::next_event_nr]" }
	{ event_sla_id "" }
	{ event_customer_contact_id "" }
	{ task_id "" }
	message:optional
	{ event_status_id "" }
	{ event_type_id "" }
	{ return_url "/intranet-events/" }
	{ vars_from_url ""}
	{ plugin_id:integer "" }
	{ view_name "event_list"}
	{ mine_p "all" }
	{ form_mode "edit" }
	{ orderby "orderby" }
        { render_template_id:integer 0 }
	{ format "html" }
    }

    set show_components_p 1
    set enable_master_p 1

} else {
    
    set task_id $task(task_id)
    set case_id $task(case_id)

    set vars_from_url ""
    set return_url [im_url_with_query]

    set event_id [db_string pid "select object_id from wf_cases where case_id = :case_id" -default ""]
    set transition_key [db_string transition_key "select transition_key from wf_tasks where task_id = :task_id"]
    set task_page_url [export_vars -base [ns_conn url] { event_id task_id return_url}]

    set show_components_p 0
    set enable_master_p 0
    set event_type_id ""
    set event_sla_id ""
    set event_customer_contact_id ""

    set plugin_id ""
    set view_name "standard"
    set mine_p "all"

    set orderby "orderby"

    set render_template_id 0
    set format "html"

    # Don't show this page in WF panel.
    # Instead, redirect to this same page, but in TaskViewPage mode.
    # ad_returnredirect "/intranet-events/new?event_id=$task(object_id)"

    # fraber 20100602: redirecting to return_url leads to an infinite
    # loop with workflow. Re-activating redirection to the EventNewPage
    # ad_returnredirect $return_url

    ad_returnredirect [export_vars -base "/intranet-events/new" { {event_id $task(object_id)} {form_mode display}} ]
}


# ------------------------------------------------------------------
# Default & Security
# ------------------------------------------------------------------


# Invalid event_id?
if {[info exists event_id] && "" == $event_id} { unset event_id }


# By default return to the existing event
# IF we are editing an existing event (event_id exists and is not empty)
set event_customer_ids [list]
if {[info exists event_id] && "" != $event_id} {
    set return_url [export_vars -base "/intranet-events/new" {event_id {form_mode display}} ]
    set event_customer_ids [db_list event_customer_ids "
	select	company_id
	from	acs_rels r,
		im_companies c
	where	r.object_id_one = :event_id and
		r.object_id_two = c.company_id	   
    "]
}

ns_log Notice "new: after ad_page_contract"

set current_user_id [ad_maybe_redirect_for_registration]
set user_id $current_user_id
set current_url [im_url_with_query]
set action_url "/intranet-events/new"
set focus "event.var_name"

set view_events_all_p [im_permission $current_user_id "view_events_all"]
set current_user_is_admin_p [im_is_user_site_wide_or_intranet_admin $current_user_id]
set copy_from_event_name ""

# message_html allows us to add a warning popup etc.
set message_html ""

if {[info exists event_id]} {
    im_event_permissions $current_user_id $event_id view_p read_p write_p admin_p
    if {!$read_p} {
        ad_return_complaint 1 "<li>You don't have the permissions to see this event."
        ad_script_abort
    }
} else {
    # Default permissions for 
    set view_p [im_permission $current_user_id "add_events"]
    set read_p $view_p
    set write_p $view_p
    set admin_p $view_p
}

# Required by im_group_member_component 
set user_admin_p $write_p

# ----------------------------------------------
# Page Title

set page_title [lang::message::lookup "" intranet-events.New_Event "New Event"]
if {[exists_and_not_null event_id]} {
    set page_title "[lang::message::lookup "" intranet-events.Event "Event"] [db_string title "select event_name from im_events where event_id = :event_id" -default ""]"
}
if {"" == $page_title && 0 != $event_type_id} { 
    set event_type [im_category_from_id $event_type_id]
    set page_title [lang::message::lookup "" intranet-events.New_EventType "New %event_type%"]
} 

set context [list $page_title]



# ----------------------------------------------
# Localization
# ----------------------------------------------

set mine_all_l10n [lang::message::lookup "" intranet-core.Mine_All "Mine/All"]
set all_l10n [lang::message::lookup "" intranet-core.All "All"]
set timescale_l10n [lang::message::lookup "" intranet-events.Timescale "Timescale"]

set show_users_l10n [lang::message::lookup "" intranet-events.Show_Users_P "Show Users?"]
set show_locations_l10n [lang::message::lookup "" intranet-events.Show_Locations_P "Show Locations?"]
set show_resources_l10n [lang::message::lookup "" intranet-events.Show_Resources_P "Show Resources?"]
set show_event_list_l10n [lang::message::lookup "" intranet-events.Show_Event_List_P "Show Event List?"]
set show_all_users_l10n [lang::message::lookup "" intranet-events.Show_All_Users_P "Show All Users?"]

set current_year [db_string current_year "select to_char(now(), 'YYYY')"]
set next_year [db_string current_year "select to_char(now(), 'YYYY')::integer + 1"]
set previous_year [db_string current_year "select to_char(now(), 'YYYY')::integer - 1"]
set show_users_l10n [lang::message::lookup "" intranet-events.Show_Users_P "Show Users?"]
set show_locations_l10n [lang::message::lookup "" intranet-events.Show_Locations_P "Show Locations?"]
set show_resources_l10n [lang::message::lookup "" intranet-events.Show_Resources_P "Show Resources?"]
set show_event_list_l10n [lang::message::lookup "" intranet-events.Show_Event_List_P "Show Event List?"]
set show_all_users_l10n [lang::message::lookup "" intranet-events.Show_All_Users_P "Show All Users?"]

set current_year [db_string current_year "select to_char(now(), 'YYYY')"]
set next_year [db_string current_year "select to_char(now(), 'YYYY')::integer + 1"]
set previous_year [db_string current_year "select to_char(now(), 'YYYY')::integer - 1"]


# ----------------------------------------------
# Determine event type

# We need the event_type_id for page title, dynfields etc.
# Check if we can deduce the event_type_id from event_id
if {0 == $event_type_id || "" == $event_type_id} {
    if {[exists_and_not_null event_id]} { 
	set event_type_id [db_string ttype_id "select event_type_id from im_events where event_id = :event_id" -default 0]
    }
}

# ----------------------------------------------
# Calculate form_mode

if {"edit" == [template::form::get_action event_action]} { set form_mode "edit" }
if {![info exists event_id]} { set form_mode "edit" }
if {![info exists form_mode]} { set form_mode "display" }

set edit_event_status_p [im_permission $current_user_id edit_event_status]
# ToDo remove
set edit_event_status_p 1

# Show the ADP component plugins?
if {"edit" == $form_mode} { set show_components_p 0 }

set event_exists_p 0
if {[exists_and_not_null event_id]} {
    # Check if the event exists
    set event_exists_p [db_string event_exists_p "select count(*) from im_events where event_id = :event_id"]

    # Write Audit Trail
    im_project_audit -project_id $event_id -action before_update

}

# Check if the event was changed recently by another user
if {"edit" == $form_mode && [info exists event_id]} {
    set exists_p [db_0or1row recently_changed "
	select	(now() - lock_date)::interval as lock_interval,
		trunc(extract(epoch from now() - lock_date))::integer % 60 as lock_seconds,
		trunc(extract(epoch from now() - lock_date) / 60.0)::integer as lock_minutes,
		lock_user,
		im_name_from_user_id(lock_user) as lock_user_name,
		lock_ip
	from	im_biz_objects
	where	object_id = :event_id
    "]

    # Check that we've found the value
    if {$exists_p && "" != $lock_user} {

	# Write out a warning if the event was modified by a different
	# user in the last 10 minutes
	set max_lock_seconds [im_parameter -package_id [im_package_core_id] LockMaxLockSeconds "" 600]
	set max_lock_seconds [im_parameter -package_id [im_package_event_id] LockMaxLockSeconds "" $max_lock_seconds]
	if {$lock_seconds < $max_lock_seconds && $lock_user != $current_user_id} {
	    
	    set msg [lang::message::lookup "" intranet-events.Event_Recently_Edited "This event was locked by %lock_user_name% %lock_minutes% minutes and %lock_seconds% seconds ago."]

	    set nonce_html ""
	    if {[info exists ::__csp_nonce] && "" ne $::__csp_nonce} {
		set nonce_html "nonce=\"$::__csp_nonce\""
	    }

	    set message_html "
		<script type=\"text/javascript\" $nonce_html>
			alert('$msg');
		</script>
	    "

	}
    } else {
	    
	# Set the lock on the event
	db_dml set_lock "
		update im_biz_objects set
			lock_ip = '[ns_conn peeraddr]',
			lock_date = now(),
			lock_user = :current_user_id
		where object_id = :event_id
        "
    }
}

# ---------------------------------------------
# The base form. Define this early so we can extract the form status
# ---------------------------------------------

set material_options [im_material_options \
			  -restrict_to_status_id 0 \
			  -restrict_to_type_id 0 \
			  -restrict_to_uom_id 0 \
			  -include_empty 1 \
			  -show_material_codes_p 2 \
			  -max_option_len 50 \
]

set location_options [im_conf_item_options \
			  -include_empty_p 1 \
			  -include_empty_name "" \
			  -type_id "" \
			  -status_id "" \
			  -type_id 81500 \
			  ]


set event_name_label [lang::message::lookup {} intranet-events.Event_Name "Name"]
set event_nr_label [lang::message::lookup {} intranet-events.Event_Nr "Nr"]
set event_name_help [lang::message::lookup {} intranet-events.Event_Name_Help {Please enter a descriptive name for the new event.}]

set edit_p [im_permission $current_user_id add_events]
set delete_p $edit_p

set actions {}
if {$edit_p} { lappend actions [list [lang::message::lookup {} intranet-events.Edit Edit] edit] }
#if {$delete_p} { lappend actions [list [lang::message::lookup {} intranet-events.Delete Delete] delete] }

ns_log Notice "new: ad_form: Setup fields"
ad_form \
    -name event_form \
    -cancel_url "/intranet-events" \
    -action $action_url \
    -actions $actions \
    -has_edit 1 \
    -mode $form_mode \
    -export {next_url return_url} \
    -form {
	event_id:key
	{event_nr:text(hidden) {label $event_nr_label} {html {size 30}} }
	{event_location_id:text(select) {label "[lang::message::lookup {} intranet-events.Location Location]"} {options $location_options}}
	{event_material_id:text(select),optional {label "[lang::message::lookup {} intranet-events.Material Material]"} {options $material_options}}
	{event_start_date:date(date) {label "[_ intranet-timesheet2.Start_Date]"} {format "YYYY-MM-DD"} {after_html {<input type="button" id=event_start_date_calendar style="height:23px; width:23px; background: url('/resources/acs-templating/calendar.gif');" >}}}
	{event_end_date:date(date) {label "[_ intranet-timesheet2.End_Date]"} {format "YYYY-MM-DD"} {after_html {<input type="button" id=event_end_date_calendar style="height:23px; width:23px; background: url('/resources/acs-templating/calendar.gif');">}}}
	{event_description:text(textarea),optional,nospell {label "[_ intranet-timesheet2.Description]"} {html {cols 40}}}
    }


# ------------------------------------------------------------------
# User Extensible Event Actions
# ------------------------------------------------------------------

set tid [value_if_exists event_id]
set event_action_html "
<form action=/intranet-events/action name=event_action>
[export_vars -form {return_url tid}]
<input type=submit value='[lang::message::lookup "" intranet-events.Action "Action"]'>
[im_category_select \
     -translate_p 1 \
     -package_key "intranet-events" \
     -plain_p 1 \
     -include_empty_p 1 \
     -include_empty_name "" \
     "Intranet Event Action" \
     action_id \
]
</form>
"

if {!$edit_event_status_p} { set event_action_html "" }


set event_elements [list]
lappend event_elements {event_type_id:text(im_category_tree) {label "[lang::message::lookup {} intranet-events.Type Type]"} {custom {category_type "Intranet Event Type" translate_p 1 package_key "intranet-events"}}}
# lappend event_elements {event_type_id:text(hidden)}

if {$edit_event_status_p} {
    lappend event_elements {event_status_id:text(im_category_tree) {label "[lang::message::lookup {} intranet-events.Status Status]"} {custom {category_type "Intranet Event Status" translate_p 1 package_key "intranet-events"}} }
} else {
    lappend event_elements {event_status_id:text(hidden),optional}
}

if {[info exists event_id]} {
    lappend event_elements {event_consultant_abbreviation:text(text),optional {label "[lang::message::lookup {} intranet-events.Consultant_Abbreviations {Consultant Abbreviation}]"}  }
    lappend event_elements {event_location_abbreviation:text(text),optional {label "[lang::message::lookup {} intranet-events.Location_Abbreviations {Location Abbreviation}]"}  }
    lappend event_elements {event_resource_abbreviation:text(text),optional {label "[lang::message::lookup {} intranet-events.Resource_Abbreviations {Resource Abbreviation}]"}  }
    lappend event_elements {event_name:text(text),optional {label $event_name_label} {html {size 80}}}
} else {
    lappend event_elements {event_name:text(hidden),optional}
}

ns_log Notice "new: ad_form: extend with event_elements"
ad_form -extend -name event_form -form $event_elements



# ---------------------------------------------
# Add DynFields to the form
# ---------------------------------------------

set dynfield_event_type_id ""
if {[info exists event_type_id]} { set dynfield_event_type_id $event_type_id}

set dynfield_event_id ""
if {[info exists event_id]} { set dynfield_event_id $event_id }

set field_cnt [im_dynfield::append_attributes_to_form \
                       -form_display_mode $form_mode \
                       -object_subtype_id $dynfield_event_type_id \
                       -object_type "im_event" \
                       -form_id event_form \
                       -object_id $dynfield_event_id \
]

# ------------------------------------------------------------------
# 
# ------------------------------------------------------------------


# Fix for problem changing to "edit" form_mode
set form_action [template::form::get_action event]
if {"" != $form_action} { set form_mode "edit" }

ns_log Notice "new: before ad_form on_request"
ad_form -extend -name event_form -on_request {
    ns_log Notice "new: on_request"

    set event_status_id 82000
    set event_type_id 82100

} -select_query {

	select	e.*
	from	im_events e
	where	e.event_id = :event_id

} -new_data {
    ns_log Notice "new: new_data"
    set event_start_date_sql [template::util::date get_property sql_date $event_start_date]
    set event_end_date_sql [template::util::date get_property sql_date $event_end_date]

    # Logic for automatically calculating an event_name
    if {"" == [string trim $event_name]} {
	set start_date $event_start_date
	if {[regexp {^([0-9]{4}) ([0-9]{1,2}) ([0-9]{1,2})} $event_start_date match year month day]} { set start_date "$year-$month-$day" }

	set end_date $event_end_date
	if {[regexp {^([0-9]{4}) ([0-9]{1,2}) ([0-9]{1,2})} $event_end_date match year month day]} { set end_date "$year-$month-$day" }

	set material_name [acs_object_name $event_material_id]
	set location_name [acs_object_name $event_location_id]
	set event_name "$material_name; $location_name; $start_date; $end_date; $event_nr"
	# Event 35; SolidWorks Erweiterte Bauteilmodellierung; SolidLine Ludwigsburg; 2014-03-03; 2014-03-04
    }

    set event_id [db_string event_insert {}]
    db_dml event_update {}
    db_dml event_update_acs_object {}

    im_dynfield::attribute_store \
	-object_type "im_event" \
	-object_id $event_id \
	-form_id event_form
    
    # Workflow?
    set wf_key [db_string wf "select trim(aux_string1) from im_categories where category_id = :event_type_id" -default ""]
    set wf_exists_p [db_string wf_exists "select count(*) from wf_workflows where workflow_key = :wf_key"]
    if {$wf_exists_p} {
	set context_key ""
	set case_id [wf_case_new \
			 $wf_key \
			 $context_key \
			 $event_id \
			]
	
	# Determine the first task in the case to be executed and start+finisch the task.
	im_workflow_skip_first_transition -case_id $case_id
    }

    # Write Audit Trail
    im_project_audit -project_id $event_id -action after_create

    # Return JSON when called from REST interface
    if {"json" == $format} { 
	doc_return 200 "application/json" "{\"success\": true}" 
	ad_script_abort
    }

    ad_returnredirect [export_vars -base "/intranet-events/new" {event_id {form_mode display}}]
    ad_script_abort

} -edit_data {
    ns_log Notice "new: edit_data"

    set event_start_date_sql [template::util::date get_property sql_date $event_start_date]
    set event_end_date_sql [template::util::date get_property sql_date $event_end_date]

    # Logic for automatically calculating an event_name
    if {"" == [string trim $event_name]} {
	set start_date $event_start_date
	if {[regexp {^([0-9]{4}) ([0-9]{1,2}) ([0-9]{1,2})} $event_start_date match year month day]} { set start_date "$year-$month-$day" }

	set end_date $event_end_date
	if {[regexp {^([0-9]{4}) ([0-9]{1,2}) ([0-9]{1,2})} $event_end_date match year month day]} { set end_date "$year-$month-$day" }

	set material_name [acs_object_name $event_material_id]
	set location_name [acs_object_name $event_location_id]
	set event_name "$material_name; $location_name; $start_date; $end_date; $event_nr"
	# Event 35; SolidWorks Erweiterte Bauteilmodellierung; SolidLine Ludwigsburg; 2014-03-03; 2014-03-04
    }

    db_dml event_update {}
    db_dml event_update_acs_object {}

    im_dynfield::attribute_store \
	-object_type "im_event" \
	-object_id $event_id \
	-form_id event_form

    # Write Audit Trail
    im_project_audit -project_id $event_id -action after_update

} -on_submit {

	ns_log Notice "new: on_submit"

} -after_submit {

    ns_log Notice "new: after_submit"

    # Reset the lock on the event
    db_dml set_lock "
	update im_biz_objects set
		lock_ip = null,
		lock_date = null,
		lock_user = null
	where object_id = :event_id
    "

    if {"json" == $format} { 
	doc_return 200 "application/json" "{\"success\": true}" 
	ad_script_abort
    }
    ad_returnredirect $return_url
    ad_script_abort

} -validate {
    {event_name
	{ [string length $event_name] < 200 }
	"[lang::message::lookup {} intranet-events.Event_name_too_long {Event Name too long (max 100 characters).}]" 
    }
    {event_name
        {![db_string event_count "select count(*) from im_events where event_name = :event_name and event_id != :event_id"]}
	"[lang::message::lookup {} intranet-events.Event_name_already_exists {Event Name already exists}]" 
    }
    {event_nr
        {![db_string event_count "select count(*) from im_events where event_nr = :event_nr and event_id != :event_id"]}
	"[lang::message::lookup {} intranet-events.Event_nr_already_exists {Event Nr already exists}]" 
    }
    {event_end_date
        {"t" == [db_string event_dates "select [template::util::date get_property sql_date $event_start_date]::date <= [template::util::date get_property sql_date $event_end_date]::date"]}
	"[lang::message::lookup {} intranet-events.Event_end_date_after_start {End Date should end after start date}]" 
    }
}


# Generate im_timesheet_task entries for each event
if {[info exists event_id]} {
    im_event::task_sweeper -event_id $event_id
}



# ---------------------------------------------------------------
# Event Menu
# ---------------------------------------------------------------

# Setup the subnavbar
set bind_vars [ns_set create]
ns_set put $bind_vars event_id [im_opt_val -limit_to integer event_id]
ns_set put $bind_vars form_mode [im_opt_val -limit_to alnum form_mode]


if {![info exists event_id]} { set event_id "" }

set event_parent_menu_id [db_string parent_menu "select menu_id from im_menus where label='events'" -default 0]
set sub_navbar [im_sub_navbar \
    -components \
    -current_plugin_id $plugin_id \
    -base_url "/intranet-events/new?event_id=$event_id" \
    -plugin_url "/intranet-events/new" \
    $event_parent_menu_id \
    $bind_vars "" "pagedesriptionbar" "summary"] 


# ---------------------------------------------------------------
# Notifications
# ---------------------------------------------------------------

set notification_html ""

if {$show_components_p} {

    set notification_object_id $event_id
    set notification_delivery_method_id [notification::get_delivery_method_id -name "email"]
    set notification_interval_id [notification::get_interval_id -name "instant"]
    set notification_type_short_name "event_notif"
    set notification_type_pretty_name "Event Notification"
    set notification_title [string totitle $notification_type_pretty_name]
    set notification_type_id [notification::type::get_type_id -short_name $notification_type_short_name]
    set notification_current_url [im_url_with_query]
    
    # Check if subscribed
    set notification_request_id [notification::request::get_request_id \
				     -type_id $notification_type_id \
				     -object_id $notification_object_id \
				     -user_id $user_id]
    
    set notification_subscribed_p [expr ![empty_string_p $notification_request_id]]
    
    if { $notification_subscribed_p } {
	set notification_url [notification::display::unsubscribe_url -request_id $notification_request_id -url $notification_current_url]
    } else {
	set notification_url [export_vars -base "/notifications/request-new?" {
	    {object_id $notification_object_id} 
	    {type_id $notification_type_id}
	    {delivery_method_id $notification_delivery_method_id}
	    {interval_id $notification_interval_id}
	    {"form\:id" "subscribe"}
	    {formbutton\:ok "OK"}
	    {return_url $notification_current_url}
	}]
    }
    
    set notification_message [ad_decode $notification_subscribed_p 1 "Unsubscribe from $notification_type_pretty_name" "Subscribe to $notification_type_pretty_name"]
    set printer_friendly_url [export_vars -base "/intranet-events/new" {event_id return_url {render_template_id 1}}]
    set printer_friendly_message [lang::message::lookup "" intranet-events.Show_in_printer_friendly_format "Show in printer friendly format"]
	
    set notification_html "
	<ul>
	<li><a href=\"$notification_url\">$notification_message</a>
	<li><a href=\"$printer_friendly_url\">$printer_friendly_message</a>
	</ul>
    "
}






# ---------------------------------------------------------------
# Filter with Dynamic Fields
# ---------------------------------------------------------------

set dynamic_fields_p 1
set form_id "event_filter"
set object_type "im_event"
set action_url "/intranet-events/index"
set form_mode "edit"

set today [db_string today "select now()::date from dual"]
set start_date $today


# Defaults for filters
set report_show_users_p 1
set report_show_locations_p 1
set report_show_resources_p 1
set report_show_event_list_p 0
set report_show_all_users_p 0




set mine_p_options {}
if {$view_events_all_p} { 
    lappend mine_p_options [list $all_l10n "all" ] 
}
lappend mine_p_options [list [lang::message::lookup "" intranet-events.Mine "Mine"] "mine"]

# Add custom searches to drop-down
if {[im_table_exists im_sql_selectors]} {
    set selector_sql "
	select	s.name, s.short_name
	from	im_sql_selectors s
	where	s.object_type = :object_type
    "
    db_foreach selectors $selector_sql {
	lappend mine_p_options [list $name $short_name]
    }
}


set timescale_types [list \
                         "next_3w" [lang::message::lookup "" intranet-timesheet2.Next_3_Weeks "Next 3 Weeks"] \
                         "next_3m" [lang::message::lookup "" intranet-timesheet2.Next_3_Month "Next 3 Months"] \
			 "until_end_of_year" [lang::message::lookup "" intranet-timesheet2.Until_end_of_year "Until end of year"] \
			 "current_year" [lang::message::lookup "" intranet-timesheet2.Current_year "Current Year"] \
			 "next_year" [lang::message::lookup "" intranet-timesheet2.Next_year "Next Year"] \
			 "previous_year" [lang::message::lookup "" intranet-timesheet2.Previous_year "Previous Year"] \
			 "since_start_of_year" [lang::message::lookup "" intranet-timesheet2.Since_start_of_year "Since start of year"] \
                         "future" [lang::message::lookup "" intranet-timesheet2.Future "Future"] \
                         "past" [lang::message::lookup "" intranet-timesheet2.Past "Past"] \
                         "last_3m" [lang::message::lookup "" intranet-timesheet2.Last_3_Month "Last 3 Months"] \
                         "last_3w" [lang::message::lookup "" intranet-timesheet2.Last_3_Weeks "Last 3 Weeks"] \
]
foreach { value text } $timescale_types {
    lappend timescale_options [list $text $value]
}


set event_member_options [util_memoize [list db_list_of_lists event_members "
	select  distinct
		im_name_from_user_id(object_id_two) as user_name,
		object_id_two as user_id
	from    acs_rels r,
		im_events p
	where   r.object_id_one = p.event_id
	order by user_name
"] 300]
set event_member_options [linsert $event_member_options 0 [list $all_l10n ""]]
set event_creator_options [list]
set event_creator_options [db_list_of_lists event_creators "
	select	distinct
		im_name_from_user_id(creation_user) as creator_name,
		creation_user as creator_id
	from	acs_objects
	where	object_type = :object_type
	order by creator_name
"]
set event_creator_options [linsert $event_creator_options 0 [list "" ""]]

set material_options [im_material_options \
			 -restrict_to_status_id 0 \
			 -restrict_to_type_id 0 \
			 -restrict_to_uom_id 0 \
			 -include_empty 1 \
			 -show_material_codes_p 1 \
]

set cost_center_options [im_cost_center_options \
			     -include_empty 1 \
			     -include_empty_name "" \
			     -department_only_p 0 \
			     -cost_type_id "" \
]


set event_modificator_options [list]
set event_modificator_options [util_memoize [list db_list_of_lists event_modificators "
	select	distinct
		im_name_from_user_id(modifying_user) as modificator_name,
		modifying_user as modificator_id
	from	acs_objects
	where	object_type = '$object_type'
	order by modificator_name
"]]
set event_modificator_options [linsert $event_modificator_options 0 [list "" ""]]





ad_form \
    -name $form_id \
    -action $action_url \
    -mode $form_mode \
    -method GET \
    -form {
	{start_date:text(text) {label "[_ intranet-timesheet2.Start_Date]"} {html {size 10}} {after_html {<input type="button" id=start_date_calendar style="height:20px; width:20px; background: url('/resources/acs-templating/calendar.gif');" >}}}
    	{timescale:text(select),optional {label "$timescale_l10n"} {options $timescale_options }}
	{report_event_selection:text(hidden),optional}
	{report_user_selection:text(hidden),optional}
	{report_location_selection:text(hidden),optional}
	{report_resource_selection:text(hidden),optional}
	{report_show_event_list_p:integer(checkbox),optional {label "$show_event_list_l10n"} {options {{"" 1}}} }
	{event_name:text(text),optional {label "[_ intranet-core.Name]"} {html {size 12}}}
	{event_material_id:text(select),optional {label "[lang::message::lookup {} intranet-events.Material Material]"} {options $material_options} }
	{event_cost_center_id:text(select),optional {label "[lang::message::lookup {} intranet-events.Cost_Center {Cost Center}]"} {options $cost_center_options} }
	{event_status_id:text(im_category_tree),optional {label "[lang::message::lookup {} intranet-events.Status Status]"} {custom {category_type "Intranet Event Status" translate_p 1 package_key "intranet-core"}} }
    }




if {$view_events_all_p} {  
    ad_form -extend -name $form_id -form {
	{event_type_id:text(im_category_tree),optional {label "[lang::message::lookup {} intranet-events.Type Type]"} {custom {category_type "Intranet Event Type" translate_p 1 package_key "intranet-core"} } }
	{event_creator_id:text(select),optional {label "[lang::message::lookup {} intranet-events.Creator Creator]"} {options $event_creator_options}}
	{event_modificator_id:text(select),optional {label "[lang::message::lookup {} intranet-events.Modificator Modificator]"} {options $event_modificator_options}}
    }

    template::element::set_value $form_id event_status_id [im_opt_val -limit_to integer event_status_id]
    template::element::set_value $form_id event_type_id [im_opt_val -limit_to integer event_type_id]
}

template::element::set_value $form_id start_date [im_opt_val -limit_to nohtml start_date]
template::element::set_value $form_id timescale [im_opt_val -limit_to nohtml timescale]
template::element::set_value $form_id event_material_id [im_opt_val -limit_to nohtml event_material_id]
template::element::set_value $form_id event_cost_center_id [im_opt_val -limit_to nohtml event_cost_center_id]
template::element::set_value $form_id report_event_selection [im_opt_val -limit_to nohtml report_events_selection]
template::element::set_value $form_id report_user_selection [im_opt_val -limit_to nohtml report_users_selection]
template::element::set_value $form_id report_location_selection [im_opt_val -limit_to nohtml report_location_selection]
template::element::set_value $form_id report_resource_selection [im_opt_val -limit_to nohtml report_resource_selection]
template::element::set_value $form_id report_show_event_list_p [im_opt_val -limit_to nohtml report_show_event_list_p]




im_dynfield::append_attributes_to_form \
    -object_type $object_type \
    -form_id $form_id \
    -object_id 0 \
    -advanced_filter_p 1 \
    -search_p 1 \
    -page_url "/intranet-events/index"

# Set the form values from the HTTP form variable frame
set org_mine_p $mine_p
im_dynfield::set_form_values_from_http -form_id $form_id
im_dynfield::set_local_form_vars_from_http -form_id $form_id
set mine_p $org_mine_p

# A customer should not get the "My queue" filter pre-selected - should he get the "My queue" selection at all? 
if { [im_profile::member_p -profile_id [im_customer_group_id] -user_id $current_user_id] && [string first "mine" [string tolower $mine_p_options]] != -1 } {
    template::element::set_value $form_id mine_p "mine"
    set mine_p "mine"
}

array set extra_sql_array [im_dynfield::search_sql_criteria_from_form \
			       -form_id $form_id \
			       -object_type $object_type
]




# ----------------------------------------------------------
# Do we have to show administration links?
# ----------------------------------------------------------

ns_log Notice "new: Before admin links"
set admin_html "<ul>"

if {[im_permission $current_user_id "add_events"]} {
    append admin_html "<li><a href=\"/intranet-events/new\">[lang::message::lookup "" intranet-events.Add_a_new_event "New Event"]</a>\n"
    append admin_html "<li><a href=\"[export_vars -base "/intranet-events/clone" {event_id return_url}]\">[lang::message::lookup "" intranet-events.Clone_this_eventt "Clone this Event"]</a>\n"

    set wf_oid_col_exists_p [im_column_exists wf_workflows object_type]
    if {$wf_oid_col_exists_p} {
	set wf_sql "
		select	t.pretty_name as wf_name,
			w.*
		from	wf_workflows w,
			acs_object_types t
		where	w.workflow_key = t.object_type
			and w.object_type = 'im_event'
	"
	db_foreach wfs $wf_sql {
	    set new_from_wf_url [export_vars -base "/intranet/events/new" {workflow_key}]
	    append admin_html "<li><a href=\"$new_from_wf_url\">[lang::message::lookup "" intranet-events.New_workflow "New %wf_name%"]</a>\n"
	}
    }
}

if {$current_user_is_admin_p} {
    append admin_html "<li><a href=\"[export_vars -base "/intranet-events/nuke" {event_id}]\">[lang::message::lookup "" intranet-events.Nuke_this_eventt "Nuke this Event"]</a>\n"    
}

# Append user-defined menus
append admin_html [im_menu_ul_list -no_uls 1 "events_admin" {}]

# Close the admin_html section
append admin_html "</ul>"


# ----------------------------------------------------------
# Link with Gantt Tasks
# ----------------------------------------------------------

set event_timesheet_task_html ""
if {[info exists event_id] && "" != $event_id && 0 != $event_id} {
    set event_timesheet_task_id [db_string ts_task_id "select event_timesheet_task_id from im_events where event_id = :event_id" -default ""]
    if {"" != $event_timesheet_task_id} {
	set ts_url [export_vars -base "/intranet-timesheet2-tasks/new" {{task_id $event_timesheet_task_id}}]
	set event_timesheet_task_html "<li><a href='$ts_url'>[lang::message::lookup "" intranet-events.See_related_timesheet_task "See related Gantt task"]</a></li>\n"
    }

    append event_timesheet_task_html "<li><a href='[export_vars -base "/intranet-events/ical.ics" {event_id}]'>[lang::message::lookup "" intranet-events.iCal_ics "iCal.ics (experimental)"]</a></li>\n"
}


# Event Project Links
set event_project_html ""
set event_order_projects [db_list_of_lists event_order_nrs "
	select distinct
		c.project_id,
		acs_object__name(c.project_id) as project_name
	from	im_companies cust,
		im_invoice_items ii,
		im_costs c,
		im_event_order_item_rels eoir
	where	ii.invoice_id = c.cost_id and
		c.customer_id = cust.company_id and
		eoir.event_id = :event_id and
		eoir.order_item_id = ii.item_id and
		c.project_id is not null
	order by
		project_name
"]
foreach tuple $event_order_projects {
    set project_id [lindex $tuple 0]
    set project_name [lindex $tuple 1]
    append event_project_html "<li><a href=[export_vars -base "/intranet/projects/view" {project_id}]>$project_name</a></li>\n"
}
if {"" != $event_project_html} {
    append event_timesheet_task_html "<li>[lang::message::lookup "" intranet-events.Related_projects "Related Projects"]:<ul>$event_project_html</ul></li>\n"
}


# Check if the sweeper got blocked somehow.
# This is until the cause has been detected.
if {[nsv_incr intranet_events sweeper_p 0] > 0} {
    set event_timesheet_task_html "<li><a href='[export_vars -base "/intranet-events/reset-sweeper-semaphore" {return_url}]'>[lang::message::lookup "" intranet-events.Semaphore_blocked_reset "The 'task_sweeper' semaphore is blocked by an internal error, please unblock."]</a></li>\n"    
}

if {"" != $event_timesheet_task_html} {
    set event_timesheet_task_html "<ul>$event_timesheet_task_html</ul>\n"
}


# ----------------------------------------------------------
# Navbars
# ----------------------------------------------------------

# Compile and execute the formtemplate if advanced filtering is enabled.
eval [template::adp_compile -string {<formtemplate id="event_filter"></formtemplate>}]
set form_html $__adp_output

set left_navbar_html "
	    <div class='filter-block'>
		<div class='filter-title'>
		    [lang::message::lookup "" intranet-events.Filter_Events "Filter Events"]
		</div>
		$form_html
	    </div>
	    <hr/>
"

    append left_navbar_html "
	    <div class='filter-block'>
		<div class='filter-title'>
		    [lang::message::lookup "" intranet-events.Admin_Events "Admin Events"]
		</div>
		$admin_html
	    </div>
	    <hr/>
    "


# ---------------------------------------------------------------
# Special Output: Format using a template
# ---------------------------------------------------------------

# Use a specific template ("render_template_id") to render the "preview"
if {0 != $render_template_id} {

    if {1 == $render_template_id} {
	# Default Template

	set template_from_param [im_parameter -package_id [im_package_event_id] DefaultEventTemplate "" ""]
	if {"" == $template_from_param} {
            # Use the default template that comes as part of the module
            set template_body "default.adp"
            set template_path "[acs_root_dir]/packages/intranet-events/templates/"
        } else {
            # Use the user's template in the template path
            set template_body $template_from_param
            set template_path [im_parameter -package_id [im_package_invoices_id] InvoiceTemplatePathUnix "" "/tmp/templates/"]
        }
    } else {
	set template_body [im_category_from_id $render_template_id]
	set template_path [im_parameter -package_id [im_package_invoices_id] InvoiceTemplatePathUnix "" "/tmp/templates/"]
    }

    if {"" == $template_body} {
        ad_return_complaint 1 "<li>You haven't specified a template for your event."
        ad_script_abort
    }

    set template_path "${template_path}/${template_body}"

    if {![file isfile $template_path] || ![file readable $template_path]} {
        ad_return_complaint "Unknown Event Template" "
        <li>Event template'$template_path' doesn't exist or is not readable
        for the web server. Please notify your system administrator."
        ad_script_abort
    }

    # Extract a few more fields for the template
    db_1row event_info "
	select	p.*,
		t.*,
		im_name_from_id(event_customer_contact_id) as event_customer_contact_name,
		im_name_from_id(event_assignee_id) as event_assignee_name,
		im_category_from_id(event_type_id) as event_type,
		im_category_from_id(event_status_id) as event_status,
		im_category_from_id(event_prio_id) as event_prio,
		sla.project_name as sla_name,
		cuc.*,
		(select country_name from country_codes where iso = cuc.ha_country_code) as ha_country_name,
		(select country_name from country_codes where iso = cuc.wa_country_code) as wa_country_name
	from	im_projects p,
		im_events t,
		im_projects sla,
		users_contact cuc
	where	p.project_id = t.event_id and
		t.event_id = :event_id and
		p.parent_id = sla.project_id and
		t.event_customer_contact_id = cuc.user_id
    "

    set forum_html [im_forum_full_screen_component -object_id $event_id -read_only_p 1]

    set user_locale [lang::user::locale]
    set locale $user_locale


    # Render the page using the template
    set invoices_as_html [ns_adp_parse -file $template_path]

    # Show invoice using template
    ns_return 200 text/html $invoices_as_html
    ad_script_abort
}

