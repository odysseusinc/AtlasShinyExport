# https://api.ohdsi.org/WebAPI/ir/1747742/report/SYNPUF5PCT?targetId=1787788&outcomeId=1783511

# https://api.ohdsi.org/WebAPI/ir/1747742/report/SYNPUF5PCT?targetId=1787664&outcomeId=1783511

# https://api.ohdsi.org/WebAPI/ir/1747742/report/SYNPUF5PCT?targetId=1787664&outcomeId=1772380

# https://api.ohdsi.org/WebAPI/ir/1747742/report/SYNPUF5PCT?targetId=1783572&outcomeId=1772380

SYNPUF5PCT_targetId1783572_outcomeId1772380

# https://api.ohdsi.org/WebAPI/ir/1747742/report/SYNPUF5PCT?targetId=1783572&outcomeId=1785347

SYNPUF5PCT_targetId1783572_outcomeId1785347



#incidence rate result example
# we have one json for every data source, target, outcome pair
d1 <- '{
  "summary": {
    "targetId": 2157,
    "outcomeId": 2158,
    "totalPersons": 1817,
    "timeAtRisk": 9148,
    "cases": 13
  },
  "stratifyStats": [],
  "treemapData": "{\"name\" : \"Everyone\", \"children\" : [{\"name\" : \"Group 0\", \"children\" : [{\"name\": \"0\", \"size\": 1817, \"cases\": 13, \"timeAtRisk\": 9148 }]}]}"
}'

d2 <- '{
  "summary": {
    "targetId": 2159,
    "outcomeId": 2158,
    "totalPersons": 289916,
    "timeAtRisk": 1045173,
    "cases": 2436
  },
  "stratifyStats": [],
  "treemapData": "{\"name\" : \"Everyone\", \"children\" : [{\"name\" : \"Group 0\", \"children\" : [{\"name\": \"0\", \"size\": 289916, \"cases\": 2436, \"timeAtRisk\": 1045173 }]}]}"
}'

# parse the data

jsonlite::fromJSON(d2)


jsonlite::read_json()



