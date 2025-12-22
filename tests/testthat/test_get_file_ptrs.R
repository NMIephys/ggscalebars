test_that("ptrs work from subdirs", {
  
  # make sure that no data_files_path is set
  options(data_files_path = NULL)
  
  testdir <- withr::local_tempdir()
  withr::with_dir(testdir, {
    
    file.copy(ephysdata::examplefile("NaV"), to=".")
    ephysdata<-read_PATCHMASTER("VG_Blocker.dat")
    
    # now, obviously, we should be able to get the traces: 
    expect_s3_class({
        ephysdata %>% head(1) %>% get_trace() 
    }, "data.frame")
    
    # but if we move into a subdir, we expect an error
    dir.create("subdir", showWarnings = F)
    setwd("./subdir")
    
    expect_error(
      ephysdata %>% head(1) %>% get_trace()  
    )
    
    # but with searchfolder set, it will be found
    expect_s3_class({
      set_file_searchfolder(list("./", "../", "../../"))
      ephysdata %>% head(1) %>% get_trace()  
    }, "data.frame")
    
    # we are still in the subdir, so if we copy our testfile here, it will now be in 2 places,
    # which should produce a warning:
    expect_warning({
      file.copy(ephysdata::examplefile("NaV"), to=".")
      set_file_searchfolder(list("./", "../", "../../"))
      ephysdata %>% head(1) %>% get_trace()
    }, "file was found in more than one place")
  })
  
  #cleanup after test
  options(data_files_path = NULL)
  
})


ephysdata<-read_PATCHMASTER(ephysdata::examplefile("NaV"))
ephysdata[1,"ptrs"]$ptrs[[1]]$file
fhkhf

