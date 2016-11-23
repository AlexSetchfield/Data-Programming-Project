library(testthat)
library(workflow)

# DNAaa() test
context("DNAaatestscases")
test_that("DNAaa test cases", {
  expect_that(DNAaa("GTTCAT"), prints_text("gene length = 6 bp", "protein sequence = VH", "number of amino acid= 2"))
  expect_that(DNAaa("gttcat"), prints_text("gene length = 6 bp", "protein sequence = VH", "number of amino acid= 2"))
  expect_that(DNAaa("123gttcat"), prints_text("gene length = 6 bp", "protein sequence = VH", "number of amino acid= 2"))
  })

# mw() test
context("mwtests")
test_that("mw test cases", {
  expect_that(mw("VH"), prints_text("aa sequence length =  2", "Molecular Weight prediction = 0.29 kDa"))
  expect_that(mw("vh"), prints_text("aa sequence length =  2", "Molecular Weight prediction = 0.29 kDa"))
  expect_that(mw("123vh"), prints_text("aa sequence length =  2", "Molecular Weight prediction = 0.29 kDa"))
  })

# gram.calc() test
context("gram.calctests")
test_that("gram.calc test output", {
  expect_equivalent(gram.calc("DTT", 5, 25, 58.44), "DTT = 0.007305 g")
})

# OD600() test
context("OD600tests")
test_that("OD600 test output", {
  expect_that(OD600(1.0, accurate = T, 100, 0.3, 200, 0.5), prints_text("Need 6 Hour 40 min to reach OD 1"))
  expect_that(OD600(1.0, accurate = F, 100), prints_text("Need 12 Hour 55 min to reach OD 1"))
  expect_that(OD600(1.0, 100), prints_text("Need 12 Hour 55 min to reach OD 1"))
  })

# lbmasscalc() test
context("lbmasscalctests")
test_that("lbmasscalc test output", {
  masses <- lbmasscalc(TrisHCl.conc = 50, NaCl.conc = 200, DTT.conc = 5, PMSF.conc = 1, volume = 1000)
  expect_that(masses$"Tris Mass (g)", equals(6.057))
  expect_that(masses$"NaCl Mass (g)", equals(11.688))
  expect_that(masses$"DTT Mass (g)", equals(0.771265))
  expect_that(masses$"PMSF Mass (g)", equals(0.17419))
})
