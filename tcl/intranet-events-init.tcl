ad_library {

    Initialization for intranet-events module

    @author Frank Bergmann (frank.bergmann@project-open.com)
    @creation-date 10 April, 2012
    @cvs-id $Id$
}

# Initialize the search "semaphore" to 0.
# There should be only one thread indexing files at a time...
nsv_set intranet_events sweeper_p 0

# Check for changed events every X minutes
ad_schedule_proc -thread t [parameter::get_from_package_key -package_key intranet-events -parameter EventTaskSweeperInterval -default 60] im_event::task_sweeper 
