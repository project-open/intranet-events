# /packages/intranet-events/www/reset-sweeper-semaphore.tcl
#
# Copyright (c) 1998-2008 ]project-open[
# All rights reserved

# ---------------------------------------------------------------
# 1. Page Contract
# ---------------------------------------------------------------

ad_page_contract {
    @author frank.bergmann@event-open.com
} {
    return_url
}

# Reduce the semaphore
nsv_incr intranet_events sweeper_p -1


ad_returnredirect $return_url
