
i_s <-  1:length(list)
out_tib <- tibble(tibble_a, list = NA)
for(i in i_s){
  out_tib$list[i] <- in_list[[i]]
}



i_s <-  1:length(mmpd_null_dist)
out_tib <- tibble(sim_null_neat, list = NA)
for(i in i_s){
  out_tib$list[i] <- mmpd_null_dist[[i]]
}