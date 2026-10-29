test_that("introduction navigation reports explicit, repeatable destinations", {
  shiny::testServer(introductionServer, {
    session$flushReact()
    expect_null(session$getReturned()())
    session$setInputs(to_material = 1)
    expect_identical(session$getReturned()(), list(page = "transmission", sequence = 1L))
    session$setInputs(zu_Import1 = 1)
    expect_identical(session$getReturned()(), list(page = "import", sequence = 2L))
    session$setInputs(to_explanations = 1)
    expect_identical(session$getReturned()(), list(page = "explanations", sequence = 3L))
    session$setInputs(to_material = 2)
    expect_identical(session$getReturned()(), list(page = "transmission", sequence = 4L))
  })
})

test_that("introduction navigation starts fresh in a second session", {
  shiny::testServer(introductionServer, {
    session$flushReact()
    expect_null(session$getReturned()())
    session$setInputs(to_explanations = 1)
    expect_identical(session$getReturned()(), list(page = "explanations", sequence = 1L))
  })
})
