# Correlation & modeling with NaverShopping and KMA datasets
- a repo for correlation test and modeling with Naver shopping insight API data and KMA observations

## Datasets
- KMA observations
  - [기상청 기상자료개방포털](https://data.kma.go.kr/)
- Naver Shopping Insight API data
  - [Shopping Insight API document](https://developers.naver.com/docs/datalab/shopping/)
  - [Naver Datalab/Shopping Insight](https://datalab.naver.com/shoppingInsight/sCategory.naver)

## How to run the project
### run any script of interest individually
- [NSI dataset] Execute the following command in the RStudio console:
```
system(paste("src/save_NaverShopping_data.R", **Naver-Client-Id**, **Naver-Client-Secret**))
```
- [KMA observation] Execute the following command in the RStudio console:
  - 'config/KMA_params.json' should be present in its place (refer to config/KMA_params_example.json)
  - for KMA ASOS hourly observation dataset:
```
sys.source('src/retrieve_KMA_API_data.R', envir=new.env(parent=parent.frame()))
```
- [R Markdown] Execute the following command in the RStudio console:
```
source('src/make_results.R')
```
### run all scripts at once
- Execute the following command in the RStudio console:
```
source('src/run_all_scripts.R')
```
