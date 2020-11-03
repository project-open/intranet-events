<master>
<property name="title">@page_title@</property>
<property name="context">#intranet-core.context#</property>
<property name="main_navbar_label">events</property>
<property name="sub_navbar">@event_navbar_html;noquote@</property>
<property name="left_navbar">@left_navbar_html;noquote@</property>

<!-- Show calendar on start- and end-date -->
<script type="text/javascript" <if @::__csp_nonce@ not nil>nonce="@::__csp_nonce;literal@"</if>>
window.addEventListener('load', function() { 
     document.getElementById('start_date_calendar').addEventListener('click', function() { showCalendar('start_date', 'y-m-d'); });
});

window.addEventListener('load', function() { 
     document.getElementById('list_check_all').addEventListener('click', function() { acs_ListCheckAll('event', this.checked) });
});

</script>


<style>
#target {
  position: absolute;
  left: 50px;
  top: 300px;
  border: 2px solid black;
  background-color: #ddd;
}
</style>

<if "" ne @event_cube_html@>
<%= [im_box_header $page_title] %>
@event_cube_html;noquote@
<%= [im_box_footer] %>
</if>

<if @report_show_event_list_p@>
<table cellspacing=0 cellpadding=0 border=0 width="100%">
<form action=/intranet-events/action method=POST>
<%= [export_vars -form {return_url}] %>
<tr valign=top>
<td>

	<table class="table_list_page">
	<%= $table_header_html %>
	<%= $table_body_html %>
	<%= $table_continuation_html %>
<!--	%= $table_submit_html %   -->
	</table>


</td>
<td width="<%= $dashboard_column_width %>">
<%= $dashboard_column_html %>
</td>
</tr>
</form>
</table>
</if>

