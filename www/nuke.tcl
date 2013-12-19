# /packages/intranet-helpdesk/www/nuke.tcl
#
# Copyright (c) 2013 ]project-open[
#

ad_page_contract {
    Create a copy of an event based on another event
    @author frank.bergmann@project-open.com
} {
    { return_url ""}
    event_id:integer
}

set current_user_id [ad_maybe_redirect_for_registration]
im_event_permissions $current_user_id $event_id view_p read_p write_p admin_p
if {!$admin_p} {
    ad_return_complaint 1 "<b>[lang::message::lookup "" intranet-events.Nuke_Event "Nuke Event"]</b>:<br>
    [lang::message::lookup "" intranet-events.No_permissions "You don't have permissions to nuke this event."]
    "
}


if {"" == $return_url} {
    set return_url [export_vars -base "/intranet-events/index"]
}

# ---------------------------------------------------------
# Nuke Relationships
# ---------------------------------------------------------

set rels [db_list rels "
	select	r.rel_id,
		r.rel_type,
		r.object_id_one,
		r.object_id_two
	from	acs_rels r
	where	r.object_id_one = :event_id
	order by r.rel_id
"]

foreach rel_tuple $rels {
    set rel_id [lindex $rel_tuple 0]
    set rel_type [lindex $rel_tuple 1]
    set rel_o1 [lindex $rel_tuple 2]
    set rel_o2 [lindex $rel_tuple 3]

    db_dml del_bom "delete from im_biz_object_members where rel_id = :rel_id"
    db_dml del_bom "delete from im_event_customer_rels where rel_id = :rel_id"
    db_dml del_rels "delete from acs_rels where rel_id = :rel_id"
    db_dml del_rels "delete from acs_objects where object_id = :rel_id"
}



# ---------------------------------------------------------
# Nuke im_event_order_item_rels
# ---------------------------------------------------------

db_dml del_order_item_rel "delete from im_event_order_item_rels where event_id = :event_id"


# ---------------------------------------------------------
# Nuke event
# ---------------------------------------------------------

db_string nule_event "select im_event__delete(:event_id)"



ad_returnredirect $return_url

