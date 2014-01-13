# /packages/intranet-events/www/ical.ics.tcl
#
# Copyright (c) 2013 ]project-open[ & SolidLine AG
#
#
#
# All rights reserved. Please check
# http://www.project-open.com/license/ for details.



ad_page_contract {
        @author frank.bergmann@project-open.com mharms@solidline.de
} {
    event_id:integer
}

# -----------------------------------------------------------
# Permissions
# -----------------------------------------------------------

set current_user_id [ad_maybe_redirect_for_registration]
im_event_permissions $current_user_id $event_id view_p read_p write_p admin_p
if {!$read_p} {
    ad_return_complaint 1 "<li>You don't have the permissions to see this event."
    ad_script_abort
}

db_1row event_info "
select	t.*,
	im_name_from_user_id(organizer_id) as organizer_name,
	im_email_from_user_id(organizer_id) as organizer_email
from	
	
	(select	e.*,
		cat.category,
		m.material_name,
		to_char(e.event_start_date, 'YYYY-MM-DD') as event_start_date_date,
		to_char(e.event_end_date, 'YYYY-MM-DD') as event_end_date_date,
		acs_object__name(e.event_location_id) as event_location_name,
		(select	min(r.object_id_two)
		from	acs_rels r,
			im_biz_object_members bom
		where	r.object_id_one = e.event_id and
			r.rel_id = bom.rel_id and
			bom.object_role_id = 1308
		) as organizer_id
	from	im_events e, im_materials m, im_categories cat
	where	e.event_id = :event_id
		and m.material_id = e.event_material_id
		and e.event_type_id = cat.category_id

	) t
"

set participants_info ""
set participants_info_sql "
select	pe.first_names || ' ' || pe.last_name as participant_name,
		u.user_id as participant_id,
		im_category_from_id(bom.member_status_id) as participant_status,
		bom.note as bom_note,
		bom.order_item_id as bom_order_item_id,
		company_name
	from	persons pe,
		parties pa,
		users u,
		acs_rels r,
		im_biz_object_members bom,
		im_companies comp
	where	r.rel_id = bom.rel_id and
		r.object_id_two = u.user_id and
		r.object_id_one = :event_id and
		comp.company_id = (select min(company_id) from im_companies, acs_rels where object_id_one = company_id and object_id_two = u.user_id) and
		u.user_id = pe.person_id and
		u.user_id = pa.party_id and
		u.user_id in (
			select	member_id
			from	group_distinct_member_map
			where	group_id = [im_profile_customers]  
		)
"

set participant_count 0 
db_foreach sql $participants_info_sql {
  append participants_info "<li>Teilnehmer $participant_name, Firma $company_name</br>"
  incr participant_count
}

set participating_companies_info ""
set participating_companies_sql "
	select company_name from im_companies, acs_rels where object_id_one = :event_id and object_id_two = company_id
"
db_foreach sql $participating_companies_sql {
  append participating_companies_info "$company_name</br>"
}

set event_start_hour_minute_second [parameter::get_from_package_key -package_key "intranet-events" -parameter "EventDefaultStartHour" -default "08:00:00"]
set event_end_hour_minute_second [parameter::get_from_package_key -package_key "intranet-events" -parameter "EventDefaultEndHour" -default "17:00:00"]

set event_start_date_pretty "$event_start_date_date $event_start_hour_minute_second"
set event_end_date_pretty "$event_end_date_date $event_end_hour_minute_second"

# ad_return_complaint 1 "$event_start_date - $event_start_date_pretty"

set event_description_mapped [string map { \n "<br />" \r "<br />"} $event_description] 


set DTSTART [calendar::outlook::ics_timestamp_format -timestamp $event_start_date_pretty]
set DTEND [calendar::outlook::ics_timestamp_format -timestamp $event_end_date_pretty]
regexp {^([0-9]*)T} $DTSTART all CREATION_DATE
set DESCRIPTION ""
set DESCRIPTION_html  "<b>Event</b><br />"
append DESCRIPTION_html "<a href='http://10.144.1.199/intranet-events/new?form_mode=display&event_id=$event_id'>$event_name</a><br />"
append DESCRIPTION_html "<br /><b>Ort</b><br />"
append DESCRIPTION_html "$event_location_name<br />"
append DESCRIPTION_html "<br /><b>Beschreibung</b><br />"
append DESCRIPTION_html "$event_description_mapped<br />"
append DESCRIPTION_html "<br /><b>Artikel</b><br />"
append DESCRIPTION_html "$material_name<br />"
append DESCRIPTION_html "<br /><b>Typ</b><br />"
append DESCRIPTION_html "$category<br />"
append DESCRIPTION_html "<br /><b>Event Kunden</b><br />"
append DESCRIPTION_html "$participating_companies_info<br />"
append DESCRIPTION_html "<br /><b>Event Teilnehmer ($participant_count)</b><br />"
append DESCRIPTION_html "$participants_info<br /><br />"
append DESCRIPTION_html "<a href='https://de.surveymonkey.com/s/Seminarleiter_SLAG'>Seminarbewertung für den Seminarleiter</a><br />"
append DESCRIPTION_html "<a href='http://10.144.1.199/intranet/download/home/0/Teilnehmerliste_Leistungsnachweis/Teilnehmerliste.xlsm'>Teilnehmerliste / Zertifikat / Namensschilder</a><br />"

set TITLE $event_name


# Put it together
set ics_event ""
append ics_event "BEGIN:VCALENDAR\r\n"
append ics_event "PRODID:-//OpenACS//OpenACS 5.0 MIMEDIR//EN\r\n"
append ics_event "VERSION:2.0\r\n"
append ics_event "METHOD:PUBLISH\r\n"
append ics_event "BEGIN:VEVENT\r\n"
append ics_event 	"DTSTART:$DTSTART\r\n"
append ics_event 	"DTEND:$DTEND\r\n"

# ORGANIZER leads to error in MS-Project
# append ics_event 	"ORGANIZER;CN=\"$organizer_name\":$organizer_email\r\n"

append ics_event 	"LOCATION:$event_location_name\r\n"
append ics_event    "X-ALT-DESC;FMTTYPE=text/html:<!DOCTYPE HTML PUBLIC '-//W3C//DTD HTML 3.2//E
    N'>\\n<HTML>\\n<HEAD>\\n<META NAME='Generator' CONTENT='MS Exchange Server ve
    rsion 08.00.0681.000'>\\n</HEAD>\\n<BODY>$DESCRIPTION_html</HTML>\r\n"
	
append ics_event 	"TRANSP:OPAQUE\r\n"
append ics_event 	"SEQUENCE:0\r\n"
append ics_event 	"UID:$event_id\r\n"
append ics_event 	"DTSTAMP:$CREATION_DATE\r\n"
append ics_event 	"DESCRIPTION:$DESCRIPTION\r\n"
append ics_event 	"SUMMARY:$TITLE\r\n"
append ics_event 	"PRIORITY:5\r\n"
append ics_event 	"CLASS:PUBLIC\r\n"
append ics_event "END:VEVENT\r\n"
append ics_event "END:VCALENDAR\r\n"


# METHOD:REQUEST
#
# METHOD:PUBLISH

# -----------------------------------------------------------
# Write out the ical file with right HTTP headers
# -----------------------------------------------------------

doc_return 200 "text/calendar" $ics_event

set content_type "text/calendar"
set filename "$event_name.ics"
set all_the_headers "HTTP/1.0 200 OK\nMIME-Version: 1.0\nContent-Type: $content_type\nContent-Disposition: attachment; filename=${filename}\r\n"
util_WriteWithExtraOutputHeaders $all_the_headers
ns_startcontent -type $content_type

ns_write $ics_event
