# /packages/intranet-events/www/ical.ics.tcl
#
# Copyright (c) 2013 ]project-open[
#
# All rights reserved. Please check
# http://www.project-open.com/license/ for details.


ad_page_contract {
        @author frank.bergmann@project-open.com
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
	select	e.*,
		to_char(e.event_start_date, 'YYYY-MM-DD') as event_start_date_date,
		to_char(e.event_end_date, 'YYYY-MM-DD') as event_end_date_date,
		acs_object__name(e.event_location_id) as event_location_name
	from	im_events e
	where	e.event_id = :event_id
"

set event_start_hour_minute_second [parameter::get_from_package_key -package_key "intranet-events" -parameter "EventStartHour" -default "08:00:00"]
set event_end_hour_minute_second [parameter::get_from_package_key -package_key "intranet-events" -parameter "EventStartHour" -default "17:00:00"]

set event_start_date_pretty "$event_start_date_date $event_start_hour_minute_second"
set event_end_date_pretty "$event_end_date_date $event_end_hour_minute_second"

# ad_return_complaint 1 "$event_start_date - $event_start_date_pretty"

set DTSTART [calendar::outlook::ics_timestamp_format -timestamp $event_start_date_pretty]
set DTEND [calendar::outlook::ics_timestamp_format -timestamp $event_end_date_pretty]

# Put it together
set ics_event "BEGIN:VCALENDAR\r\nPRODID:-//OpenACS//OpenACS 5.0 MIMEDIR//EN\r\nVERSION:2.0\r\nMETHOD:PUBLISH\r\nBEGIN:VEVENT\r\nDTSTART:$DTSTART\r\nDTEND:$DTEND\r\n"

regexp {^([0-9]*)T} $DTSTART all CREATION_DATE
set DESCRIPTION $event_description
set TITLE $event_name

append ics_event "LOCATION:$event_location_name\r\nTRANSP:OPAQUE\r\nSEQUENCE:0\r\nUID:$event_id\r\nDTSTAMP:$CREATION_DATE\r\nDESCRIPTION:$DESCRIPTION\r\nSUMMARY:$TITLE\r\nPRIORITY:5\r\nCLASS:PUBLIC\r\n"

append ics_event "END:VEVENT\r\nEND:VCALENDAR\r\n"


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

