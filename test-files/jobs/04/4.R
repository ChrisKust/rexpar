Sys.sleep(0.000000)
options(BatchJobs.on.slave = TRUE, BatchJobs.resources.path = 'C:/Users/kustosz/Documents/R/rexpar_package/rexpar/test-files/resources/resources_1426599593.RData')
library(checkmate)
library(BatchJobs)
res = BatchJobs:::doJob(
	reg = loadRegistry('C:/Users/kustosz/Documents/R/rexpar_package/rexpar/test-files'),
	ids = c(4L),
	multiple.result.files = FALSE,
	disable.mail = FALSE,
	first = 13L,
	last = 8L,
	array.id = NA)
BatchJobs:::setOnSlave(FALSE)