# /packages/intranet-events/www/biz-card-add.tcl
#
# Copyright (c) 1998-2008 ]project-open[
# All rights reserved

# ---------------------------------------------------------------
# 1. Page Contract
# ---------------------------------------------------------------

ad_page_contract {
    @author frank.bergmann@event-open.com
} {
    user_id:integer
    also_add_users
    return_url
}

# ---------------------------------------------------------------
# Defaults & Security
# ---------------------------------------------------------------

set event_id [lindex $also_add_users 0]
set role_id [lindex $also_add_users 1]
im_security_alert_check_integer -location "/intranet-events/biz-card-add" -value $role_id

set current_user_id [ad_maybe_redirect_for_registration]
im_event_permissions $current_user_id $event_id view read write admin
if {!$write} {
    ad_return_complaint 1 "You don't have the right to modify this event"
    ad_script_abort
}

set rel_id [im_biz_object_add_role $user_id $event_id $role_id]
db_dml update_rel "update im_biz_object_members set member_status_id = [im_event_participant_status_reserved] where rel_id = :rel_id"


# Mark the event as changed and call callback
db_dml update_object "
	update acs_objects set 
	   	last_modified = now(),
	   	modifying_user = :current_user_id,
		modifying_ip = '[ad_conn peeraddr]'
	where object_id = :event_id
"

# Audit the object
im_audit -object_id $event_id -action "after_update"



ad_returnredirect $return_url
