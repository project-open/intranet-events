<if @enable_master_p@><master></if>
<property name="title">@page_title@</property>
<property name="context">@context;noquote@</property>
<property name="main_navbar_label">events</property>
<property name="focus">@focus;noquote@</property>
<property name="sub_navbar">@sub_navbar;noquote@</property>
<property name="left_navbar">@left_navbar_html;noquote@</property>

@message_html;noquote@

<!-- Show calendar on start- and end-date -->
<script type="text/javascript" <if @::__csp_nonce@ not nil>nonce="@::__csp_nonce;literal@"</if>>
window.addEventListener('load', function() { 
     document.getElementById('start_date_calendar').addEventListener('click', function() { showCalendar('start_date', 'y-m-d'); });
     document.getElementById('event_start_date_calendar').addEventListener('click', function() { showCalendar('event_start_date', 'y-m-d'); });
     document.getElementById('event_end_date_calendar').addEventListener('click', function() { showCalendar('event_end_date', 'y-m-d'); });
});
</script>

<if @message@ not nil>
    <div class="general-message">@message@</div>
</if>
<if @view_name@ eq "component">
    <%= [im_component_page -plugin_id $plugin_id -return_url "/intranet-events/new?event_id=$event_id"] %>
</if>
<else>
    <if @show_components_p@>
    <%= [im_component_bay top] %>
    <table width="100%">
	<tr valign="top">
	<td width="50%">
		<%= [im_box_header "[lang::message::lookup "" intranet-events.Event_Details "Event Details"]"] %>
		<formtemplate id="event_form"></formtemplate>
		@event_timesheet_task_html;noquote@

<!--		@event_action_html;noquote@
		@notification_html;noquote@
-->
		<%= [im_box_footer] %>
		<%= [im_component_bay left] %>
	</td>
	<td width="50%">
		<%= [im_component_bay right] %>
	</td>
	</tr>
    </table>
    <%= [im_component_bay bottom] %>
    </if>
    <else>

            <table width="100%">
                <tr valign="top">
                <td>
		    <%= [im_box_header $page_title] %>
		    <formtemplate id="event_form"></formtemplate>
		    <%= [im_box_footer] %>
                </td>
                <td>
			<%= [im_component_bay new_right] %> <!-- ToDo: validate -->
                </td>
                </tr>
            </table>

    </else>
</else>

