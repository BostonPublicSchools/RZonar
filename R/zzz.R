.onLoad <- function(libname, pkgname) {
  rlang::run_on_load()
}

.zonarCache <- NULL
rlang::on_load(
  .zonarCache <- cachem::cache_disk(dir = ".zonarCache", max_size = 1024^4)
)
