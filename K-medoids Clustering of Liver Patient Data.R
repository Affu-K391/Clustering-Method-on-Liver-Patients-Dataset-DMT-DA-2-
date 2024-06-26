data <- read.table(text = "
Age	Gender	Total_Bilirubin	Direct_Bilirubin	Alkaline_Phosphotase	Alamine_Aminotransferase	Aspartate_Aminotransferase	Total_Protiens	Albumin	Albumin_and_Globulin_Ratio	Dataset
65	Female	0.7	0.1	187	16	18	6.8	3.3	0.9	1
62	Male	10.9	5.5	699	64	100	7.5	3.2	0.74	1
62	Male	7.3	4.1	490	60	68	7	3.3	0.89	1
58	Male	1	0.4	182	14	20	6.8	3.4	1	1
72	Male	3.9	2	195	27	59	7.3	2.4	0.4	1
46	Male	1.8	0.7	208	19	14	7.6	4.4	1.3	1
26	Female	0.9	0.2	154	16	12	7	3.5	1	1
29	Female	0.9	0.3	202	14	11	6.7	3.6	1.1	1
17	Male	0.9	0.3	202	22	19	7.4	4.1	1.2	2
55	Male	0.7	0.2	290	53	58	6.8	3.4	1	1
57	Male	0.6	0.1	210	51	59	5.9	2.7	0.8	1
72	Male	2.7	1.3	260	31	56	7.4	3	0.6	1
64	Male	0.9	0.3	310	61	58	7	3.4	0.9	2
74	Female	1.1	0.4	214	22	30	8.1	4.1	1	1
61	Male	0.7	0.2	145	53	41	5.8	2.7	0.87	1
25	Male	0.6	0.1	183	91	53	5.5	2.3	0.7	2
38	Male	1.8	0.8	342	168	441	7.6	4.4	1.3	1
33	Male	1.6	0.5	165	15	23	7.3	3.5	0.92	2
40	Female	0.9	0.3	293	232	245	6.8	3.1	0.8	1
40	Female	0.9	0.3	293	232	245	6.8	3.1	0.8	1
51	Male	2.2	1	610	17	28	7.3	2.6	0.55	1
51	Male	2.9	1.3	482	22	34	7	2.4	0.5	1
62	Male	6.8	3	542	116	66	6.4	3.1	0.9	1
40	Male	1.9	1	231	16	55	4.3	1.6	0.6	1
63	Male	0.9	0.2	194	52	45	6	3.9	1.85	2
34	Male	4.1	2	289	875	731	5	2.7	1.1	1
34	Male	4.1	2	289	875	731	5	2.7	1.1	1
34	Male	6.2	3	240	1680	850	7.2	4	1.2	1
20	Male	1.1	0.5	128	20	30	3.9	1.9	0.95	2
84	Female	0.7	0.2	188	13	21	6	3.2	1.1	2
57	Male	4	1.9	190	45	111	5.2	1.5	0.4	1
52	Male	0.9	0.2	156	35	44	4.9	2.9	1.4	1
57	Male	1	0.3	187	19	23	5.2	2.9	1.2	2
38	Female	2.6	1.2	410	59	57	5.6	3	0.8	2
38	Female	2.6	1.2	410	59	57	5.6	3	0.8	2
30	Male	1.3	0.4	482	102	80	6.9	3.3	0.9	1
17	Female	0.7	0.2	145	18	36	7.2	3.9	1.18	2
46	Female	14.2	7.8	374	38	77	4.3	2	0.8	1
48	Male	1.4	0.6	263	38	66	5.8	2.2	0.61	1
47	Male	2.7	1.3	275	123	73	6.2	3.3	1.1	1
45	Male	2.4	1.1	168	33	50	5.1	2.6	1	1
62	Male	0.6	0.1	160	42	110	4.9	2.6	1.1	2
42	Male	6.8	3.2	630	25	47	6.1	2.3	0.6	2
50	Male	2.6	1.2	415	407	576	6.4	3.2	1	1
85	Female	1	0.3	208	17	15	7	3.6	1	2
35	Male	1.8	0.6	275	48	178	6.5	3.2	0.9	2
21	Male	3.9	1.8	150	36	27	6.8	3.9	1.34	1
40	Male	1.1	0.3	230	1630	960	4.9	2.8	1.3	1
32	Female	0.6	0.1	176	39	28	6	3	1	1
55	Male	18.4	8.8	206	64	178	6.2	1.8	0.4	1
45	Female	0.7	0.2	170	21	14	5.7	2.5	0.7	1
34	Female	0.6	0.1	161	15	19	6.6	3.4	1	1
38	Male	3.1	1.6	253	80	406	6.8	3.9	1.3	1
38	Male	1.1	0.3	198	86	150	6.3	3.5	1.2	1
42	Male	8.9	4.5	272	31	61	5.8	2	0.5	1
42	Male	8.9	4.5	272	31	61	5.8	2	0.5	1
33	Male	0.8	0.2	198	26	23	8	4	1	2
48	Female	0.9	0.2	175	24	54	5.5	2.7	0.9	2
51	Male	0.8	0.2	367	42	18	5.2	2	0.6	1
64	Male	1.1	0.5	145	20	24	5.5	3.2	1.39	2
31	Female	0.8	0.2	158	21	16	6	3	1	1
58	Male	1	0.5	158	37	43	7.2	3.6	1	1
58	Male	1	0.5	158	37	43	7.2	3.6	1	1
57	Male	0.7	0.2	208	35	97	5.1	2.1	0.7	1
57	Male	1.3	0.4	259	40	86	6.5	2.5	0.6	1
57	Male	1.4	0.7	470	62	88	5.6	2.5	0.8	1
54	Male	2.2	1.2	195	55	95	6	3.7	1.6	1
37	Male	1.8	0.8	215	53	58	6.4	3.8	1.4	1
66	Male	0.7	0.2	239	27	26	6.3	3.7	1.4	1
60	Male	0.8	0.2	215	24	17	6.3	3	0.9	2
19	Female	0.7	0.2	186	166	397	5.5	3	1.2	1
75	Female	0.8	0.2	188	20	29	4.4	1.8	0.6	1
75	Female	0.8	0.2	205	27	24	4.4	2	0.8	1
52	Male	0.6	0.1	171	22	16	6.6	3.6	1.2	1
68	Male	0.7	0.1	145	20	22	5.8	2.9	1	1
29	Female	0.7	0.1	162	52	41	5.2	2.5	0.9	2
31	Male	0.9	0.2	518	189	17	5.3	2.3	0.7	1
68	Female	0.6	0.1	1620	95	127	4.6	2.1	0.8	1
70	Male	1.4	0.6	146	12	24	6.2	3.8	1.58	2
58	Female	2.8	1.3	670	48	79	4.7	1.6	0.5	1
58	Female	2.4	1.1	915	60	142	4.7	1.8	0.6	1
29	Male	1	0.3	75	25	26	5.1	2.9	1.3	1
49	Male	0.7	0.1	148	14	12	5.4	2.8	1	2
33	Male	2	1	258	194	152	5.4	3	1.25	1
32	Male	0.6	0.1	237	45	31	7.5	4.3	1.34	1
14	Male	1.4	0.5	269	58	45	6.7	3.9	1.4	1
13	Male	0.6	0.1	320	28	56	7.2	3.6	1	2
58	Male	0.8	0.2	298	33	59	6.2	3.1	1	1
18	Male	0.6	0.2	538	33	34	7.5	3.2	0.7	1
60	Male	4	1.9	238	119	350	7.1	3.3	0.8	1
60	Male	5.7	2.8	214	412	850	7.3	3.2	0.78	1
60	Male	6.8	3.2	308	404	794	6.8	3	0.7	1
60	Male	8.6	4	298	412	850	7.4	3	0.6	1
60	Male	5.8	2.7	204	220	400	7	3	0.7	1
60	Male	5.2	2.4	168	126	202	6.8	2.9	0.7	1
75	Male	0.9	0.2	282	25	23	4.4	2.2	1	1
39	Male	3.8	1.5	298	102	630	7.1	3.3	0.8	1
39	Male	6.6	3	215	190	950	4	1.7	0.7	1
18	Male	0.6	0.1	265	97	161	5.9	3.1	1.1	1
", header = TRUE, stringsAsFactors = FALSE)
clustering_data <- data[, c("Age", "Total_Bilirubin", "Direct_Bilirubin", "Alkaline_Phosphotase", "Alamine_Aminotransferase", "Aspartate_Aminotransferase", "Total_Protiens", "Albumin", "Albumin_and_Globulin_Ratio")]
library(cluster)
kmedoids_result <- pam(clustering_data, k = 3)  
print(kmedoids_result$medoids)
print(kmedoids_result$clustering)
library(ggplot2)
data$cluster <- as.factor(kmedoids_result$clustering)
ggplot(data, aes(x = Total_Bilirubin, y = Alkaline_Phosphotase, color = cluster)) +
  geom_point(size = 3) +
  labs(title = "K-medoids Clustering of Liver Patient Data",
       x = "Total Bilirubin",
       y = "Alkaline Phosphotase",
       color = "Cluster") +
  theme_minimal()

