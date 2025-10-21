# === Create color schemes =========================
colors_coverage_bins <- c("95-100%"="#1a9641",
                          "90-94%"="#feb24c",
                          "85-89%"="#ef3b2c",
                          "<85%" = "#a50f15")

colors_conversion_bins <- c("90-100%"="#1a9641",
                            "70-89%"="#feb24c",
                            "<70%"="#d7191c")

colors_coverage_bins2 <- c(">100%"="#006d2c",
                           "95-100%"="#74c476",
                           "90-94%"="#feb24c",
                           "85-89%"="#ef3b2c",
                           "<85%" = "#a50f15")

colors_clusters_lt95_bin <- c("<5%" = "#1a9641",
                              "5-10%" = "#ffffbf",
                              "11-25%" = "#fdae61",
                              ">25%" = "#d7191c")

colors_awareness_bins <- c( "75-100%" = "#1a9641",
                            "50-74%" = "#ffffbf",
                            "25-49%" = "#fdae61",
                            "0-24%" = "#d7191c")

colors_passfail_bins <- c("All Lots Passed" = "#1a9641",
                          "One or More Lots Failed" = "#d7191c",
                          "Incomplete Data" = "#ffffbf")

colors_passfail_bins2 <- c("Passed Lots" = "#1a9641",
                           "Failed Lots" = "#d7191c")

colors_modality_bins <- c("M2M" = "#e41a1c",
                          "S2S" = "#ff7f00",
                          "M2M/S2S" = "#ffff33",
                          "H2H" = "#4daf4a",
                          "H2H/M2M" = "#377eb8",
                          "H2H/S2S" = "#984ea3",
                          "H2H/M2M/S2S" = "#a6562c")

colors_reasons_missed_pca <- c("Absent" = "#ccebc5", 
                               "Newborn/Sleeping/Sick" = "#beaed4", 
                               "Refusal" = "#ffff99", 
                               "Team did not Visit" = "#f0027f", 
                               "Not Aware" = "#386cb0", 
                               "Vaccination site is too far" = "#bf5b17", 
                               "Child Not Available" = "#7fc97f",
                               "No One Available to Take Child to Site" = "#984ea3", 
                               "Other" = "#666666")

colors_reasons_missed_admin <- c("Child Absent (Return During Campaign)" = "#ccebc5", 
                                 "Child Absent (Return After Campaign)" = "#33a02c",
                               "Newborn/Sleeping/Sick" = "#beaed4", 
                               "Refusal" = "#ffff99", 
                               "Other" = "#666666")


colors_reasons_missed_lqas <- c("Absent" = "#7fc97f", 
                                "Newborn/Sleeping/Sick" = "#beaed4", 
                                "Refusal" = "#ffff99", 
                                "Team did not Visit" = "#f0027f", 
                                "Other" = "#666666",
                                "Expected Home Visit" = "violet",
                                "No Vaccine at Site" = "orange",
                                "Parent Forgot Or No Time" = "#984ea3",
                                "Not Aware" = "#386cb0",
                                "Vaccination site is too far" = "#bf5b17",
                                "Very Long Queue" = "darkred")

colors_reasons_missed_ooh <- c("Absent" = "#7fc97f", 
                               "Newborn/Sleeping/Sick" = "#beaed4", 
                               "Refusal" = "#ffff99", 
                               "Team did not Visit" = "#f0027f", 
                               "Not Aware" = "#386cb0", 
                               "Vaccination site is too far" = "#bf5b17", 
                               "No One Available to Take Child to Site" = "#984ea3",
                               "Other" = "#666666")

colors_awareness_source_pca <- c("Social Mobilizer" = "#a6cee3",
                                 "Community Health Worker" = "#b2df8a",
                                 "Community Elder" = "#a65628",
                                 "Mullah" = "#ffff99",
                                 "Radio" = "#fb9a99",
                                 "TV" = "#e31a1c",
                                 "Poster" = "#fdbf6f",
                                 "Teacher" = "#cab2d6",
                                 "Other" = "#666666")

colors_door_marking_pca <- c("Correct" = "#4daf4a",
                             "Incorrect" = "#ff7f00", 
                             "Not Marked" = "#e41a1c")

colors_wastage_bins <- c("< -10%"= "#5e3c99",
                         "-9-0%" = "#b2abd2",
                         "0-9%" = "#ffffbf",
                         "10-19%" = "#fdb863",
                         ">20%" = "#e66101")

colors_hrmp_pct_bins <- c("0%"= "#ffffcc",
                          "<1%" = "#a1dab4",
                          "1-4%" = "#41b6c4",
                         "5-9%" = "#2c7fb8",
                         "10%+" = "#253494")

colors_site_density_bins <- c("<10 vaccinated per site" = "#1a9641",
                              "10-15 vaccinated per site" = "#a6d96a",
                              "16-20 vaccinated per site" = "#ffffbf",
                              "21-25 vaccinated per site" = "#fdae61",
                              "26+ vaccinated per site" = "#d7191c")

colors_clusters_consistently_low_bins <- c( "0" = "#abd9e9",
                                            "1" = "#ffffb2",
                                            "2-3" = "#fed976",
                                            "4-5" = "#fd8d3c",
                                            "6+" = "#bd0026",
                                            "Insufficient Data" = "#d9d9d9")

colors_validation <- c("100%" = "#1a9850",
                       "90-99%" = "#ffffbf",
                       "70-89%" = "#fee08b",
                       "50-69%" = "#fc8d59",
                       "1-49%" = "#d73027",
                       "0%" = "#67000d")

colors_verification_bins <- c("Not Verified or Published" = "#d7191c",
                              "Verified but not Published" = "#ffffbf",
                              "Published and Verified" = "#1a9641")

colors_refusal_rate_bins <- c("<1%" = "#1a9641",
                              "1-5%" = "#ffffbf",
                              "6-10%" = "#fdae61",
                              ">10%" = "#d7191c")

colors_reasons_missed_pca_absent <- c(
  ">40"   = "#2f6843",
  "20-40" = "#70a380",
  "1-19"  = "#b4d3c0",
  "0"     = "#ffffff"
)

colors_reasons_missed_pca_newborn_sleeping_sick <- c(
  ">40"   = "#5f3b78",
  "20-40" = "#9971b1",
  "1-19"  = "#ceb4dd",
  "0"     = "#ffffff"
)

colors_reasons_missed_pca_refusal <- c(
  ">40"   = "#948c00",
  "20-40" = "#dcd847",
  "1-19"  = "#f1ef91",
  "0"     = "#ffffff"
)

colors_reasons_missed_pca_team_did_not_visit <- c(
  ">40"   = "#800040",
  "20-40" = "#cc5a92",
  "1-19"  = "#efbad5",
  "0"     = "#ffffff"
)

colors_reasons_missed_pca_not_aware <- c(
  ">40"   = "#16396c",
  "20-40" = "#5079b0",
  "1-19"  = "#9fbbe0",
  "0"     = "#ffffff"
)

colors_reasons_missed_pca_vaccination_site_is_too_far <- c(
  ">40"   = "#59290c",
  "20-40" = "#b06b3b",
  "1-19"  = "#e5bb97",
  "0"     = "#ffffff"
)

colors_reasons_missed_pca_child_not_available <- c(
  ">40"   = "#2f662f",
  "20-40" = "#6faa6f",
  "1-19"  = "#b4dcb4",
  "0"     = "#ffffff"
)

colors_reasons_missed_pca_no_one_available_to_take_child_to_site <- c(
  ">40"   = "#4b1f5a",
  "20-40" = "#8c5aa3",
  "1-19"  = "#c9a7d7",
  "0"     = "#ffffff"
)

colors_reasons_missed_pca_other <- c(
  ">40"   = "#16514e",
  "20-40" = "#5aa29e",
  "1-19"  = "#aad7d5",
  "0"     = "#ffffff"
)

colors_reasons_missed_pca_general <- c(
  ">40"   = "#cc4c02",
  "20-40" = "#fe9929",
  "1-19"  = "#fed98e",
  "0"     = "#ffffff"
)
