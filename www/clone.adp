<master>
<property name="title">@page_title@</property>
<property name="main_navbar_label"></property>

<%= [im_box_header $page_title] %>

<form action='/intranet-events/clone-2' method=GET>
<%= [export_vars -form {return_url event_id}] %>
<table cellspacing=2 cellpadding=2>

<tr class=roweven>
    <td><%= [lang::message::lookup "" intranet-events.New_Start_Date "New start date for cloned event"] %></td>
    <td><input type=text name=clone_start_date value="@event_start_date_iso@"></td>
</tr>

<tr class=roweven>
    <td></td>
    <td><input type=submit value='<%= [lang::message::lookup "" intranet-core.Continue "Continue"] %>'></td>
</tr>

</table>
</form>
<%= [im_box_footer] %>

