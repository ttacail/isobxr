#  #_________________________________________________________________________80char
#' Merge results from all chunks of a given sweep.final_nD run
#'
#' @description  Merge results from all chunks of a given sweep.final_nD run
#'
#' @param workdir Working directory of \strong{\emph{0_ISOBXR_MASTER.xlsx}} master file, \cr
#' of the dynamic sweep master file (e.g., \strong{\emph{0_EXPLO_DYN_MASTER.xlsx}}) \cr
#' and where output files will be stored if saved by user. \cr
#' (character string)
#' @param FINnD_digest_dir.to_merge Name of sweep.final_nD digest directory
#' to which current chunks should be merged. \cr
#' For instance: "4_FINnD_0_SWEEP_FINnD_demo_001_000_digest"
#' @param save_outputs If TRUE, saves merged chunks outputs to sweep.final_nD digest directory.
#' @return Merged chunks sweep.final_nD outputs, including results, chunk logs, chunked parameter spaces.
#'
#' @export
merge_FINnD_chunks <- function(workdir,
                               FINnD_digest_dir.to_merge,
                               save_outputs = FALSE){

  # workdir <- "/Users/sz18642/isobxr Gd 2023/5_human_iK_final_nD/"
  # FINnD_digest_dir.to_merge = "4_FINnD_0_SWEEP_FINnD_demo_001_000_digest"
  # save_outputs = TRUE

  variable <- value <- sweeped_fraction_perc <- NULL

  rlang::inform(paste0("\n", "________________________________________________________________________________"))
  rlang::inform("\U0001f535 Merging the processed chunks.")

  LOC_workdir <- workdir
  old <- getwd()
  on.exit(setwd(old), add = TRUE)
  if (!dir.exists(LOC_workdir)) rlang::abort("Working directory not found.")
  setwd(LOC_workdir)

  if(isFALSE(stringr::str_ends(FINnD_digest_dir.to_merge, pattern = "0_digest"))){
    rlang::abort("FINnD_digest_dir.to_merge should be refering to a sweep_space digest folder ending with _digest.")
  }

  if(!dir.exists(FINnD_digest_dir.to_merge)){
    rlang::abort("FINnD_digest_dir.to_merge does not exist.")
  }

  digest_files <- list.files(paste0(workdir, "/", FINnD_digest_dir.to_merge))
  path.chunk_log <- digest_files[stringr::str_ends(digest_files, pattern = "_chunk_log.RDS")]
  chunk_log <- readRDS(paste0(workdir, "/", FINnD_digest_dir.to_merge, "/", path.chunk_log))
  chunk_log.complete <- chunk_log[chunk_log$chunk_status == "complete",]
  complete_chunk.dirs <- paste("4_", chunk_log.complete$chunk_ID, sep = "")

  path.evD <- paste0(complete_chunk.dirs, "/",
                     list.files(complete_chunk.dirs, pattern = "*_chunk_results.RDS"))

  path.LOG <- paste0(complete_chunk.dirs, "/",
                     list.files(complete_chunk.dirs, pattern = "*_chunk_LOG.csv"))

  path.param_space <- paste0(complete_chunk.dirs, "/",
                             list.files(complete_chunk.dirs, pattern = "*_chunk_param_space.RDS"))

  space_digest_name_root <- stringr::str_remove(list.files(FINnD_digest_dir.to_merge,
                                                           pattern = "*_param_space.RDS"),
                                                pattern = "_param_space.RDS")

  space_digest_name_root <- space_digest_name_root[!stringr::str_ends(space_digest_name_root, pattern = "_merged")]

  merged_results <- path.evD %>%
    purrr::map(base::readRDS) %>%
    purrr::reduce(dplyr::bind_rows)

  merged_LOG <- path.LOG %>%
    purrr::map(data.table::fread) %>%
    purrr::reduce(dplyr::bind_rows)

  merged_param_space <- path.param_space %>%
    purrr::map(base::readRDS) %>%
    purrr::reduce(dplyr::bind_rows)

  # plot sweep progress
  pspace.merged <- merged_param_space
  pspace.merged$index <- rownames(pspace.merged)

  pspace.all <- readRDS(base::paste0(FINnD_digest_dir.to_merge, "/", space_digest_name_root, "_param_space.RDS" ))
  pspace.all$index <- rownames(pspace.all)

  counts.pspace.all <- pspace.all %>%
    reshape2::melt(id.vars = "index", factorsAsStrings = T) %>%
    dplyr::group_by(variable, value) %>%
    dplyr::summarise(n_all_space = dplyr::n(),
                     .groups = "drop_last") %>%
    as.data.frame() %>%
    suppressWarnings()

  counts.pspace.merged <- pspace.merged[,!names(pspace.merged) %in% c("chunk_n")] %>%
    reshape2::melt(id.vars = "index", factorsAsStrings = T) %>%
    dplyr::group_by(variable, value) %>%
    dplyr::summarise(n_sweeped_merged = dplyr::n(),
                     .groups = "drop_last") %>%
    as.data.frame() %>%
    suppressWarnings()

  counts.pspace <- dplyr::full_join(counts.pspace.merged, counts.pspace.all, by = c("variable", "value"))
  counts.pspace$sweeped_fraction_perc <- 100*counts.pspace$n_sweeped_merged/counts.pspace$n_all_space

  sweep_progress <- ggplot2::ggplot(counts.pspace,
                                    ggplot2::aes(x = value,
                                                 y = sweeped_fraction_perc,
                                                 fill = sweeped_fraction_perc
                                    )) +
    ggplot2::scale_fill_gradientn(colours=c('red','green2'), limits=c(0,100))+
    ggplot2::scale_color_gradientn(colours=c('red','green2'), limits=c(0,100))+
    ggplot2::facet_wrap( . ~ variable, scales = "free_x") +
    ggplot2::theme_linedraw() +
    ggplot2::geom_line(color = "gray50", alpha = .5, linetype = 2)+
    ggplot2::geom_linerange(ggplot2::aes(ymin = 0, ymax = sweeped_fraction_perc, color = sweeped_fraction_perc), size = 4)+
    ggplot2::theme(strip.text = ggplot2::element_text(face="bold"),
                   panel.grid = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(hjust = 1, angle = 45),
                   legend.position = "None")+
    ggplot2::geom_hline(yintercept = 0, linetype = 1, color = "black", size = 0.25)+
    ggplot2::geom_hline(yintercept = 100, linetype = 1, color = "black", size = 0.25)+
    ggplot2::labs(title = "Progress of sweeps in space of parameters",
                  subtitle = paste0(dec_n(100 * nrow(pspace.merged) / nrow(pspace.all), 0), "% of space was swept and merged",
                                    " (", dec_n(nrow(pspace.merged), 0), " of ", dec_n(nrow(pspace.all),0), " sweeps)"),
                  y = "Proportion of swept values (%)",
                  x = "Parameter values")

  if (nrow(pspace.merged) == nrow(pspace.all)){
    complete_chunk.dirs %>% unlink(recursive = T)
  }

  rlang::inform("\U2705 Successful.")
  print(sweep_progress)

  if (save_outputs){
    merged_results %>%
      base::saveRDS(file = base::paste0(FINnD_digest_dir.to_merge, "/",
                                        space_digest_name_root, "_merged_results.RDS" ))

    merged_LOG %>%
      data.table::fwrite(file = base::paste0(FINnD_digest_dir.to_merge, "/",
                                             space_digest_name_root, "_merged_LOG.csv" ))

    merged_param_space %>%
      base::saveRDS(file = base::paste0(FINnD_digest_dir.to_merge, "/",
                                        space_digest_name_root, "_merged_param_space.RDS" ))

    pdf(#width = 21/2.54, height = 29.7/2.54,
      file = base::paste0(FINnD_digest_dir.to_merge, "/", space_digest_name_root, "_sweep_progress.pdf" ))
    print(sweep_progress)
    dev.off()

    rlang::inform(paste0("\U2139 Merged chunks results successfully exported to digest directory: \n  ",
                         FINnD_digest_dir.to_merge))

  } else
  {
    rlang::inform("\U2139 Merged chunks results were not exported. \n Set save_outputs = TRUE to export.")
  }



}

