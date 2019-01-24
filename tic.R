add_package_checks()

# if (Sys.getenv("id_rsa") != "") {
  # pkgdown documentation can be built optionally. Other example criteria:
  # - `inherits(ci(), "TravisCI")`: Only for Travis CI
  # - `ci()$is_tag()`: Only for tags, not for branches
  # - `Sys.getenv("BUILD_PKGDOWN") != ""`: If the env var "BUILD_PKGDOWN" is set
  # - `Sys.getenv("TRAVIS_EVENT_TYPE") == "cron"`: Only for Travis cron jobs
  get_stage("before_deploy") %>%
    add_step(step_setup_ssh())

  if (ci()$get_branch() == "master") {
    get_stage("deploy") %>%
      add_code_step(remotes::install_github("ropensci/landscapetools")) %>%
      add_code_step(rmarkdown::render("README.Rmd")) %>%
      add_step(step_push_deploy(commit_paths = "README.md", branch = "master")) %>%
      add_code_step(devtools::document()) %>%
      add_step(step_push_deploy(commit_paths = c("man/", "DESCRIPTION", "NAMESPACE", branch =  "master"))) %>%
      add_step(step_build_pkgdown()) %>%
      add_step(step_push_deploy("docs", "gh-pages"))
  }

  get_stage("after_success") %>%
    step_run_code(covr::codecov(quiet = FALSE))

# }
