# /packages/intranet-helpdesk/www/clone.tcl
#
# Copyright (c) 2013 ]project-open[
#

ad_page_contract {
    Create a copy of an event based on another event
    @author frank.bergmann@project-open.com
} {
    { return_url ""}
    event_id:optional
}

set current_user_id [ad_maybe_redirect_for_registration]
set page_title [lang::message::lookup "" intranet-events.Clone_Event "Clone Event"]
set context_bar [im_context_bar $page_title]


im_event_permissions $current_user_id $event_id view_p read_p write_p admin_p
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
		e.event_start_date::date as event_start_date_iso
	from	im_events e
	where	e.event_id = :event_id
"]} {
    ad_return_complaint 1 "<b>[lang::message::lookup "" intranet-events.Clone_Event "Clone Event"]</b>:<br>
    [lang::message::lookup "" intranet-events.Event_does_not_exist "The selected event #%event_id% does not exist."]
    "
}
