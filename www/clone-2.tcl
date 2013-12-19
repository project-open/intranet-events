# /packages/intranet-helpdesk/www/clone-2.tcl
#
# Copyright (c) 2013 ]project-open[
#

ad_page_contract {
    Create a copy of an event based on another event
    @author frank.bergmann@project-open.com
} {
    { return_url ""}
    event_id:integer
    clone_start_date
}

set current_user_id [ad_maybe_redirect_for_registration]
set page_title [lang::message::lookup "" intranet-events.Clone_Event "Clone Event"]
set context_bar [im_context_bar $page_title]
set org_event_id $event_id


im_event_permissions $current_user_id $org_event_id view_p read_p write_p admin_p
if {!$read_p} {
    ad_return_complaint 1 "<li>You don't have the permissions to see this event."
    ad_script_abort
}

set add_events_p [im_permission $current_user_id "add_events"]
if {!$add_events_p} {
    ad_return_complaint 1 "<li>You don't have the permission to create new events."
    ad_script_abort
}

if {![db_0or1row event_info "
	select	*,
		:clone_start_date::date + (e.event_end_date::date - e.event_start_date::date) as clone_end_date
	from	im_events e
	where	e.event_id = :org_event_id
"]} {
    ad_return_complaint 1 "<b>[lang::message::lookup "" intranet-events.Clone_Event "Clone Event"]</b>:<br>
    [lang::message::lookup "" intranet-events.Event_does_not_exist "The selected event #%event_id% does not exist."]
    "
}


# ---------------------------------------------------------
# Create a new base event
# ---------------------------------------------------------

set event_nr [im_event::next_event_nr]
set material_name [acs_object_name $event_material_id]
set location_name [acs_object_name $event_location_id]
set event_name "$material_name; $location_name; $clone_start_date; $clone_end_date; $event_nr"

set clone_event_id [db_string event_insert "
		SELECT im_event__new(
			NULL,
			'im_event',
			now(),
			:current_user_id,
			'[ns_conn peeraddr]',
			null,

			:event_name,
			:event_nr,
			:clone_start_date,
			:clone_end_date,
			:event_status_id,
			:event_type_id
		)
"]

db_dml event_update "
	update im_events set
		event_name		= :event_name,
		event_nr		= :event_nr,
		event_type_id		= :event_type_id,
		event_status_id		= :event_status_id,
		event_material_id	= :event_material_id,
		event_location_id	= :event_location_id,
		event_cost_center_id	= :event_cost_center_id,
		event_start_date	= :clone_start_date,
		event_end_date		= :clone_end_date,
		event_description	= :event_description,
		event_consultant_abbreviation = :event_consultant_abbreviation,
		event_location_abbreviation = :event_location_abbreviation,
		event_resource_abbreviation = :event_resource_abbreviation
	where
		event_id = :clone_event_id;
"

db_dml event_update_acs_object "
		update acs_objects set
			last_modified = now(),
			modifying_user = :current_user_id,
			modifying_ip = '[ad_conn peeraddr]'
		where object_id = :clone_event_id
"



# Workflow?
set wf_key [db_string wf "select trim(aux_string1) from im_categories where category_id = :event_type_id" -default ""]
set wf_exists_p [db_string wf_exists "select count(*) from wf_workflows where workflow_key = :wf_key"]
if {$wf_exists_p} {
    set context_key ""
    set case_id [wf_case_new \
		     $wf_key \
		     $context_key \
		     $clone_event_id \
		    ]
    
    # Determine the first task in the case to be executed and start+finisch the task.
    im_workflow_skip_first_transition -case_id $case_id
}

# Write Audit Trail
im_project_audit -project_id $clone_event_id -action after_create



# ---------------------------------------------------------
# Copy relationships
# ---------------------------------------------------------

set rel_sql "
	select	*
	from	acs_rels r
		LEFT OUTER JOIN im_biz_object_members bom ON (r.rel_id = bom.rel_id)
		-- LEFT OUTER JOIN im_event_customer_rels ecr ON (r.rel_id = ecr.rel_id)
	where	r.object_id_one = :org_event_id
	order by r.rel_id
"
db_foreach copy_rels $rel_sql {
    switch $rel_type {

	im_biz_object_member {
	    set rel_id [im_biz_object_add_role $object_id_two $clone_event_id $object_role_id]
	    db_dml update_rel "
		update im_biz_object_members set 
			percentage =		:percentage,
			skill_profile_rel_id =	:skill_profile_rel_id,
			member_status_id =   	:member_status_id,
			note =		 	:note,
			order_item_id =		:order_item_id
		where rel_id = :rel_id
	    "
	}

	im_event_customer_rel {
	    set rel_id [db_string create_rel "
			select im_event_customer_rel__new(
				null,
				:current_user_id,
				'[ns_conn peeraddr]',
				:clone_event_id,
				:object_id_two
			)
	    "]
	}

	default {
	    ad_return_complaint 1 "<b>[lang::message::lookup "" intranet-events.Clone_Event "Clone Event"]</b>:<br>
            [lang::message::lookup "" intranet-events.Unknown_rel_type "Unknown relationship type '%%rel_type%'."]
            "
	}
    }
}

# ---------------------------------------------------------
# Copy im_event_order_item_rels
# ---------------------------------------------------------

set order_item_sql "
	select	eoir.*
	from	im_event_order_item_rels eoir
	where	event_id = :org_event_id
"
db_foreach order_item_rels $order_item_sql {
    set exists_p [db_string rel_exists "select count(*) from im_event_order_item_rels where event_id = :clone_event_id and order_item_id = :order_item_id"]
    if {!$exists_p} {
	db_dml create_rel "
	        insert into im_event_order_item_rels
	        (event_id, order_item_id, order_item_amount) values
	        (:clone_event_id, :order_item_id, :order_item_amount)
        "
    }
}

set return_url [export_vars -base "/intranet-events/new" {{event_id $clone_event_id} {form_mode display}}]

ad_returnredirect $return_url

