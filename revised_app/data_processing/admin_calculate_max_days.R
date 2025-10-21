admin_max_days_cluster <- admin_all %>%
  select(campaigns, rcode, pcode, dcode, ccode, age_group, max_days) %>%
  group_by(campaigns, rcode, pcode, dcode, ccode, age_group) %>%
  summarise_all(~max(as.numeric(.), na.rm=T))

admin_max_days_district <- admin_all %>%
  select(campaigns, rcode, pcode, dcode, age_group, max_days) %>%
  group_by(campaigns, rcode, pcode, dcode, age_group) %>%
  summarise_all(~max(as.numeric(.), na.rm=T)) %>%
  ungroup()

admin_max_days_province <- admin_all %>%
  select(campaigns, rcode, pcode, age_group, max_days) %>%
  group_by(campaigns, rcode, pcode, age_group) %>%
  summarise_all(~max(as.numeric(.), na.rm=T)) %>%
  ungroup()

admin_max_days_region <- admin_all %>%
  select(campaigns, rcode, age_group, max_days) %>%
  group_by(campaigns, rcode, age_group) %>%
  summarise_all(~max(as.numeric(.), na.rm=T)) %>%
  ungroup()

admin_max_days_campaign <- admin_all %>%
  select(campaigns, age_group, max_days) %>%
  group_by(campaigns, age_group) %>%
  summarise_all(~max(as.numeric(.), na.rm=T)) %>%
  ungroup()

