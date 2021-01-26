#Bai 1
sst = 1743.281 #SST khong doi
n = 22

#5a
p1 = n - 2
ssr1 = 981.326
sse1 = sst - ssr1
msr1 = ssr1/1
mse1 = sse1/p1
f_obs_1 = msr1/mse1
f_stat_1 = qf(0.95, df1 = 1, df2 = p1)

#5b
p2 = n - 3
ssr2 = ssr1 + 190.232 #SSR of H1
ssr1 #SSR of H0
sse2 = sst - ssr2 #SSE of H1
f_obs_2 = ((sse1 -sse2)/1)/(sse2/p2)
f_stat_2= qf(0.95, df1 = 1, df2 = p2)

#5c
p3 = n - 4
ssr3 = ssr2 + 129.431 #SSR of H1
sse3 = sst - ssr3 #SSE of H1
f_obs_3 = ((sse2 -sse3)/1)/(sse3/p3)
f_stat_3 = qf(0.95, df1 = 1, df2 = p3)

#6
R_sqd_1 = 1 - sse1/sst
R_adj_1 = 1 - (sse1/(n-p1))/(sst/(n-1))
  
R_sqd_2 = 1 - sse2/sst
R_adj_2 = 1 - (sse2/(n-p2))/(sst/(n-1))
  
R_sqd_3 = 1 - sse3/sst
R_adj_3 = 1 - (sse3/(n-p3))/(sst/(n-1))
