
getWIKIdata <- function(qid)
{
  query <- paste('SELECT 
  ?wdLabel ?ps_Label ?wdpqLabel ?pq_Label {\n  
  VALUES (?company) {(wd:', qid, ')}\n  \n  
  ?company ?p ?statement .\n  ?statement ?ps ?ps_ .\n 
  \n  ?wd wikibase:claim ?p.\n  ?wd wikibase:statementProperty ?ps.\n 
  \n  OPTIONAL {\n  ?statement ?pq ?pq_ .\n 
  ?wdpq wikibase:qualifier ?pq .\n  }\n  \n  
  SERVICE wikibase:label { bd:serviceParam wikibase:language "en" }\n} 
  ORDER BY ?wd ?statement ?ps_', sep="")
  
  query_wikidata(query)
}