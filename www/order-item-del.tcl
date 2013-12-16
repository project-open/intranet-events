# /packages/intranet-events/www/customer-del.tcl
#
# Copyright (c) 1998-2008 ]project-open[
# All rights reserved

# ---------------------------------------------------------------
# 1. Page Contract
# ---------------------------------------------------------------

ad_page_contract {
    @author frank.bergmann@event-open.com
} {
    event_id:integer
    item_id:integer
    return_url
}

# ---------------------------------------------------------------
# Defaults & Security
# ---------------------------------------------------------------

set current_user_id [ad_maybe_redirect_for_registration]
im_event_permissions $current_user_id $event_id view read write admin
if {!$write} {
    ad_return_complaint 1 "You don't have the right to modify this event"
    ad_script_abort
}

db_dml del_order_item_rel "
	delete from im_event_order_item_rels
	where event_id = :event_id and order_item_id = :item_id
"


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
