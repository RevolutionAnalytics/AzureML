if(interactive()) library("testthat")


settingsFile <- AzureML.config.default
workspace <- function(..., .validate = FALSE) {
  # js <- decrypt_vault()
  # id <- js$workspace$id
  # auth <- js$workspace$authorization_token
  # AzureML::workspace(id, auth, .validate = .validate)
  decrypt_vault("azure_workspace")
}

read_vault_or_config <- function(){
  if(can_decrypt_vault()) {
    decrypt_vault()
    } else {
      read.AzureML.config(settingsFile)
    }
}

#  ------------------------------------------------------------------------

context("workspace - connect to workspace")

test_that("Can decrypt secret", {
  expect_true(can_decrypt_vault())
  ws <- decrypt_vault("azure_workspace")
  expect_is(ws, "Workspace")
})

test_that("Can connect to workspace with supplied id and auth", {
  # skip_if_missing_config(settingsFile)
  js <- read_vault_or_config()
  id <- js$workspace$id
  auth <- js$workspace$authorization_token

  expect_true(!is.null(id))
  expect_true(!is.null(auth))

  ws <- workspace(id, auth)

  expect_is(ws, c("Workspace"))
  expect_equal(ls(ws), c("datasets", "experiments", "id", "services"))
  expect_equal(ws$id, id)
})

test_that("Can connect to workspace with config file", {
  # skip_if_missing_config(settingsFile)

  ws <- workspace()

  expect_is(ws, c("Workspace"))
  expect_equal(ls(ws), c("datasets", "experiments", "id", "services"))
})


test_that("workspace() throws helpful 401 error with invalid id", {
  # AzureML:::skip_if_missing_config(settingsFile)

  .catchError <- function(expr){
    tryCatch(expr, error = function(e)e)$message
  }
  .expect_error_in <- function(object, msgs){
    if(missing(object) || is.null(object)) return(FALSE)
    ptn <- sprintf("[%s]", paste(sprintf("(%s)", msgs), collapse = "|"))
    grepl(ptn, object)
  }

  m <- .catchError(AzureML::workspace(id = "x", auth = "y", .validate = TRUE))
  msg <- c("invalid workspaceId",
           "401 (Unauthorised).  Please check your workspace ID and auth codes."
  )

  .expect_error_in(m, msg = msg)

})




