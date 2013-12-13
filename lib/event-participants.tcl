# /intranet-events/lib/event-participants.tcl
#
# Variables from calling procedure:
# 	event_id
#	return_url

# ******************************************************
# Default & Security
# ******************************************************


set current_user_id [ad_maybe_redirect_for_registration]
im_event_permissions $current_user_id $event_id view read write admin
if {!$read} { return }

set return_url [im_url_with_query]
set form_mode "display"


# ******************************************************
# 
# ******************************************************

set participant_options [db_list_of_lists participant_options "
    	select	c.company_name || ' - ' || im_name_from_user_id(u.user_id),
		user_id
	from	users u,
		persons pe,
		parties pa,
		acs_rels r,
		im_biz_object_members bom,
		im_companies c
	where	u.user_id = pe.person_id and
		u.user_id = pa.party_id and
		r.object_id_two = u.user_id and
		r.object_id_one = c.company_id and
		r.rel_id = bom.rel_id and
		c.company_id in (
			select	object_id_two
			from	acs_rels
			where	object_id_one = :event_id     
		)
		-- Exclude already existing members
		and u.user_id not in (
			select	u.user_id
			from	users u,
				acs_rels r,
				im_biz_object_members bom
			where	r.rel_id = bom.rel_id and
				r.object_id_two = u.user_id and
				r.object_id_one = :event_id
		) and
		c.company_path != 'internal' and
		u.user_id not in (select member_id from group_distinct_member_map where group_id = [im_profile_employees])
	order by
		c.company_name, pe.first_names, pe.last_name
"]

# set participant_options [linsert $participant_options 0 [list "" ""]]
set customer_options [db_list_of_lists customer_options "
	select	company_name,
		company_id
	from	im_companies c,
		acs_rels r
	where	r.object_id_two = c.company_id and
		r.object_id_one = :event_id
	order by company_name
"]

set order_item_options [db_list_of_lists order_items "
select
	cost_name || ' / ' || sort_order,
	item_id
from	im_companies cust,
	im_invoice_items ii,
	im_costs c,
	im_event_order_item_rels eoir
where	ii.invoice_id = c.cost_id and
	c.customer_id = cust.company_id and
	eoir.event_id = :event_id and
	eoir.order_item_id = ii.item_id
order by
	company_name,
	cost_name,
	sort_order,
	item_name
"]


# ******************************************************
# Create the list of all attributes of the current type
# ******************************************************

set first_names_l10n [lang::message::lookup "" intranet-core.First_Names "First Names"]
set last_name_l10n [lang::message::lookup "" intranet-core.Last_Name "Last Name"]
set email_l10n [lang::message::lookup "" intranet-core.Email Email]
set company_l10n [lang::message::lookup "" intranet-core.Company Company]
set comment_l10n [lang::message::lookup "" intranet-core.Comment Comment]
set status_l10n [lang::message::lookup "" intranet-core.Status Status]
set order_item_l10n [lang::message::lookup "" intranet-core.Order_Item "Order Item"]

list::create \
    -name participant_list \
    -multirow participant_list_multirow \
    -key company_id \
    -no_data [lang::message::lookup "" intranet-events.No_participant_associated_yet "No participant associated yet"] \
    -orderby_name "participant_orderby" \
    -elements {
	company_name {
	    label "$company_l10n" 
	    display_template {
		<a href="@participant_list_multirow.company_url@" title="@participant_list_multirow.company_mouseover@"
		>@participant_list_multirow.company_name@</a>
	    }
	    link_url_col company_url	    
	}
	first_names { 
	    label "$first_names_l10n" 
	    link_url_col participant_url
	}
	last_name { 
	    label "$last_name_l10n" 
	    link_url_col participant_url
	}
	email { 
	    label "$email_l10n" 
	    link_url_col participant_url
	}
	participant_status {
	    label "$status_l10n" 
	    display_template {
		<select name=participant_status_id.@participant_list_multirow.participant_id@>
		<option value=[im_event_participant_status_reserved] @participant_list_multirow.reserved_enabled@>[lang::message::lookup "" intranet-events.Reservied "Reserved"]</option>
		<option value=[im_event_participant_status_confirmed] @participant_list_multirow.confirmed_enabled@>[lang::message::lookup "" intranet-events.Confirmed Confirmed]</option>
		</select>
	    }
	}
	order_item_id {
	    label "$order_item_l10n" 
	    display_template {
		<select name=order_item_id.@participant_list_multirow.participant_id@>
		@participant_list_multirow.order_item_options_html;noquote@
		</select>
	    }
	}
	bom_note { 
	    label "$comment_l10n" 
	    display_template {
		<textarea name=bom_note.@participant_list_multirow.participant_id@>@participant_list_multirow.note_quoted@</textarea>
	    }
	}
	participant_delete {
	    label ""
	    display_template {
		<a href="@participant_list_multirow.delete_url@" class="button">#acs-kernel.common_Delete#</a>
	    }
	}
    } \
    -orderby {
	orderby {orderby first_names}
	first_names {orderby first_names}
	last_name {orderby last_name}
    } \
    -filters {
	form_mode {}
	event_id {}
	plugin_id {}
    }


db_multirow -extend {participant_url company_name company_url company_mouseover delete_url reserved_enabled confirmed_enabled order_item_options_html note_quoted} participant_list_multirow get_participants "
	select	*,
		u.user_id as participant_id,
		im_category_from_id(bom.member_status_id) as participant_status,
		bom.note as bom_note,
		bom.order_item_id as bom_order_item_id,
		(select min(company_id) from im_companies, acs_rels where object_id_one = company_id and object_id_two = u.user_id) as company_id
	from	persons pe,
		parties pa,
		users u,
		acs_rels r,
		im_biz_object_members bom
	where	r.rel_id = bom.rel_id and
		r.object_id_two = u.user_id and
		r.object_id_one = :event_id and
		u.user_id = pe.person_id and
		u.user_id = pa.party_id and
		u.user_id in (
			select	member_id
			from	group_distinct_member_map
			where	group_id = [im_profile_customers]  
		)
	[template::list::orderby_clause -name participant_list -orderby]
" {
    set delete_url [export_vars -base "participant-del" {event_id user_id return_url}]
    set participant_url [export_vars -base "/intranet/users/view" {user_id return_url}]
    set company_url [export_vars -base "/intranet/companies/view" {company_id return_url}]
    set company_name [acs_object_name $company_id]
    set company_key_account_id [util_memoize [list db_string key_account "select min(r.object_id_two) from acs_rels r where r.object_id_one = $company_id and r.object_id_two in (select member_id from group_distinct_member_map where group_id = [im_profile_employees])" -default ""]]
    set company_key_account_name [acs_object_name $company_key_account_id]
    set company_mouseover "$company_key_account_name"
    set reserved_enabled ""
    set confirmed_enabled ""
    if {[im_event_participant_status_reserved] == $member_status_id} { set reserved_enabled "selected" }
    if {[im_event_participant_status_confirmed] == $member_status_id} { set confirmed_enabled "selected" }

    set note_quoted [string map {"'" ""} $bom_note]

    # Order Item Options - selected depending on order_item_id
    set order_item_options_html "<option value=''></option>\n"
    foreach order_item_tuple $order_item_options {
	set order_item_name [lindex $order_item_tuple 0]
	set order_item_id [lindex $order_item_tuple 1]
	
	set selected ""
	if {$order_item_id == $bom_order_item_id} { set selected "selected" }
	append order_item_options_html "<option value=$order_item_id $selected>$order_item_name</option>\n"
    }
}
