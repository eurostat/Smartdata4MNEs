# Function partly adopted and further modified from
# https://github.com/Financial-Times/gleifr package

read_gleif_entities_from_csv <- function(file){
  #' Read GLEIF entities csv file
  #'
  #' Read the "Level 1 LEI-CDF Golden Copy File" downloaded from
  #' \url{https://www.gleif.org/en/lei-data/gleif-golden-copy/download-the-golden-copy#/}.
  #' This function will read the useful columns, clean the names and convert
  #' timestamps into dates.
  #' @param file Path to a csv file
  #' @export
  
  readr::read_csv(file = file,
                  col_types = readr::cols_only(
                    LEI = "c",
                    Entity.LegalName = "c",
                    Entity.LegalAddress.FirstAddressLine	 = "c",
                    Entity.LegalAddress.AddressNumber	 = "c",
                    Entity.LegalAddress.AddressNumberWithinBuilding	 = "c",
                    Entity.LegalAddress.MailRouting	 = "c",
                    Entity.LegalAddress.AdditionalAddressLine.1	 = "c",
                    Entity.LegalAddress.AdditionalAddressLine.2	 = "c",
                    Entity.LegalAddress.AdditionalAddressLine.3	 = "c",
                    Entity.LegalAddress.City = "c",
                    Entity.LegalAddress.Region = "c",
                    Entity.LegalAddress.Country = "c",
                    Entity.LegalAddress.PostalCode = "c",
                    Entity.HeadquartersAddress.FirstAddressLine	 = "c",
                    Entity.HeadquartersAddress.AddressNumber	 = "c",
                    Entity.HeadquartersAddress.AddressNumberWithinBuilding	 = "c",
                    Entity.HeadquartersAddress.MailRouting	 = "c",
                    Entity.HeadquartersAddress.AdditionalAddressLine.1 = "c",	
                    Entity.HeadquartersAddress.AdditionalAddressLine.2 = "c",	
                    Entity.HeadquartersAddress.AdditionalAddressLine.3	 = "c",
                    Entity.HeadquartersAddress.City	 = "c",
                    Entity.HeadquartersAddress.Region = "c",
                    Entity.HeadquartersAddress.Country = "c",
                    Entity.HeadquartersAddress.PostalCode = "c",
                    Entity.OtherAddresses.OtherAddress.1.type = "c",
                    Entity.OtherAddresses.OtherAddress.1.FirstAddressLine = "c",
                    Entity.OtherAddresses.OtherAddress.1.AddressNumber = "c",
                    Entity.OtherAddresses.OtherAddress.1.AddressNumberWithinBuilding = "c",
                    Entity.OtherAddresses.OtherAddress.1.MailRouting	 = "c",
                    Entity.OtherAddresses.OtherAddress.1.AdditionalAddressLine.1="c",
                    Entity.OtherAddresses.OtherAddress.1.AdditionalAddressLine.2="c",
                    Entity.OtherAddresses.OtherAddress.1.AdditionalAddressLine.3="c",	
                    Entity.OtherAddresses.OtherAddress.1.City	= "c",
                    Entity.OtherAddresses.OtherAddress.1.Region	 = "c",
                    Entity.OtherAddresses.OtherAddress.1.Country	 = "c",
                    Entity.OtherAddresses.OtherAddress.1.PostalCode = "c",
                    Entity.RegistrationAuthority.RegistrationAuthorityID = "c",
                    Entity.LegalJurisdiction = "c",
                    Entity.LegalForm.EntityLegalFormCode = "c",	Entity.LegalForm.OtherLegalForm = "c",
                    Entity.EntityCategory = "c",
                    Entity.EntityStatus = "c",
                    Registration.InitialRegistrationDate = "c",
                    Registration.LastUpdateDate = "c",
                    Registration.RegistrationStatus = "c",
                    Registration.ValidationSources = "c",
                    Registration.ValidationAuthority.ValidationAuthorityID = "c"
                  )) %>%
    dplyr::rename(
      lei=LEI ,
      entity_legal_name=Entity.LegalName,
      entity_address1_a=Entity.LegalAddress.FirstAddressLine,
      entity_address1_b=Entity.LegalAddress.AddressNumber,
      entity_address1_c=Entity.LegalAddress.AddressNumberWithinBuilding,
      entity_address1_d=Entity.LegalAddress.MailRouting,
      entity_address1_e=Entity.LegalAddress.AdditionalAddressLine.1,
      entity_address1_f=Entity.LegalAddress.AdditionalAddressLine.2,
      entity_address1_g=Entity.LegalAddress.AdditionalAddressLine.3,
      entity_city1=Entity.LegalAddress.City,
      entity_region1=Entity.LegalAddress.Region,
      entity_country1=Entity.LegalAddress.Country,
      entity_postal1=Entity.LegalAddress.PostalCode,
      entity_hq_address_a=Entity.HeadquartersAddress.FirstAddressLine,
      entity_hq_address_b=Entity.HeadquartersAddress.AddressNumber,
      entity_hq_address_c=Entity.HeadquartersAddress.AddressNumberWithinBuilding,
      entity_hq_address_d=Entity.HeadquartersAddress.MailRouting,
      entity_hq_address_e=Entity.HeadquartersAddress.AdditionalAddressLine.1 ,
      entity_hq_address_f=Entity.HeadquartersAddress.AdditionalAddressLine.2,
      entity_hq_address_g=Entity.HeadquartersAddress.AdditionalAddressLine.3,
      entity_hq_city=Entity.HeadquartersAddress.City,
      entity_hq_region=Entity.HeadquartersAddress.Region,
      entity_hq_country=Entity.HeadquartersAddress.Country,
      entity_hq_postal=Entity.HeadquartersAddress.PostalCode,
      entity_address2_type=Entity.OtherAddresses.OtherAddress.1.type,
      entity_address2_a=Entity.OtherAddresses.OtherAddress.1.FirstAddressLine,
      entity_address2_b=Entity.OtherAddresses.OtherAddress.1.AddressNumber,
      entity_address2_c=Entity.OtherAddresses.OtherAddress.1.AddressNumberWithinBuilding,
      entity_address2_d=Entity.OtherAddresses.OtherAddress.1.MailRouting,
      entity_address2_e=Entity.OtherAddresses.OtherAddress.1.AdditionalAddressLine.1,
      entity_address2_f=Entity.OtherAddresses.OtherAddress.1.AdditionalAddressLine.2,
      entity_address2_g=Entity.OtherAddresses.OtherAddress.1.AdditionalAddressLine.3,
      entity_city2=Entity.OtherAddresses.OtherAddress.1.City,
      entity_region2=Entity.OtherAddresses.OtherAddress.1.Region,
      entity_country2=Entity.OtherAddresses.OtherAddress.1.Country,
      entity_postal2=Entity.OtherAddresses.OtherAddress.1.PostalCode,
      entity_registration_authority_id=Entity.RegistrationAuthority.RegistrationAuthorityID,
      entity_legal_jurisdiction=Entity.LegalJurisdiction,
      entity_legal_form=Entity.LegalForm.EntityLegalFormCode,
      entity_category=Entity.EntityCategory,
      entity_status=Entity.EntityStatus,
      initial_registration_date=Registration.InitialRegistrationDate,
      last_updated_date=Registration.LastUpdateDate,
      registration_status=Registration.RegistrationStatus,
      validation_authority_sources=Registration.ValidationSources,
      validation_authority_id=Registration.ValidationAuthority.ValidationAuthorityID
    ) %>%
    dplyr::mutate(
      initial_registration_date = parse_lei_ts(initial_registration_date),
      last_updated_date = parse_lei_ts(last_updated_date)
    )
}


is_lei_unix_ts <- function(ts){
	#' Check if a string is a unix millisecond timestamp
	#'
	assertthat::assert_that(all(is.character(ts) | is.na(ts)))

	# rx_start_of_line() %>%
	# 	rx_multiple(rx_digit(), min = 11, max = 13) %>%
	# 	rx_end_of_line()

	ifelse(is.na(ts),
				 FALSE,
				 grepl("^([\\d]){11,13}$", ts, perl = TRUE))

}


parse_lei_unix_ts <- function(ts){
	#' Parse LEI Unix Timestamps
	#'
	#' LEI data has some timestamps in Unix millisecond timestamps. This functions parses them into POSIXct.
	#' @param ts Unix millisecond timestamps
	#' @return POSIXct timestamps
	#'

	assertthat::assert_that(all(is.character(ts) | is.na(ts)))


	nm_ts <- suppressWarnings(as.numeric(ts))
	as.Date(as.POSIXct(ifelse(is_lei_unix_ts(ts),
														nm_ts / 1000,
														NA),
										 origin="1970-01-01 00:00:00"))

}

is_lei_iso8601_ts <- function(ts){
	#' Check if a string is an ISO8601 time stamp
	#'
	assertthat::assert_that(all(is.character(ts) | is.na(ts)))

	# rx_start_of_line() %>%
	# 	rx_multiple(rx_digit(), min = 4, max = 4) %>%
	# 	rx_find("-") %>%
	# 	rx_multiple(rx_digit(), min = 2, max = 2) %>%
	# 	rx_find("-") %>%
	# 	rx_multiple(rx_digit(), min = 2, max = 2) %>%
	# 	rx_find("T") %>%
	# 	rx_anything()

	ifelse(is.na(ts),
				 FALSE,
				 grepl("^(\\d){4}(-)(\\d){2}(-)(\\d){2}(T)(.*)", ts, perl = TRUE))
}


parse_lei_iso8601_ts <- function(ts){
	#' Parse LEI ISO8601 Timestamps
	#'

	assertthat::assert_that(all(is.character(ts) | is.na(ts)))

	as.Date(lubridate::parse_date_time(ts, orders = c("ymdT*","ymdT*z!*"), quiet = TRUE))
}

parse_lei_ts <- function(ts){

	dplyr::case_when(is_lei_unix_ts(ts) ~ parse_lei_unix_ts(ts),
									 is_lei_iso8601_ts(ts) ~ parse_lei_iso8601_ts(ts),
									 TRUE ~ as.Date(NA))
}

read_gleif_relationships_from_csv <- function(file){
	#' Read GLEIF relationships csv file
	#'
	#' Read the "Level 1 LEI-CDF Golden Copy File" downloaded from
	#' \url{https://www.gleif.org/en/lei-data/gleif-golden-copy/download-the-golden-copy#/}.
	#' This function will read the useful columns, clean the names and convert
	#' timestamps into dates.
	#' @param file Path to a csv file
	#' @export

	readr::read_csv(file = file,
									col_types = readr::cols_only(
										Relationship.StartNode.NodeID = "c",
										Relationship.StartNode.NodeIDType = "c",
										Relationship.EndNode.NodeID = "c",
										Relationship.EndNode.NodeIDType = "c",
										Relationship.RelationshipType = "c",
										Relationship.RelationshipStatus = "c",
										Relationship.Period.1.startDate = "c",
										Relationship.Period.1.endDate = "c",
										Relationship.Period.1.periodType = "c",
										Relationship.Period.2.startDate = "c",
										Relationship.Period.2.endDate = "c",
										Relationship.Period.2.periodType = "c",
										Relationship.Period.3.startDate = "c",
										Relationship.Period.3.endDate = "c",
										Relationship.Period.3.periodType = "c",
										Relationship.Period.4.startDate = "c",
										Relationship.Period.4.endDate = "c",
										Relationship.Period.4.periodType = "c",
										Relationship.Period.5.startDate = "c",
										Relationship.Period.5.endDate = "c",
										Relationship.Period.5.periodType = "c"
									)) %>%
		dplyr::rename(
			start_node_id = Relationship.StartNode.NodeID,
			start_node_type = Relationship.StartNode.NodeIDType,
			end_node_id = Relationship.EndNode.NodeID,
			end_node_type = Relationship.EndNode.NodeIDType,
			relationship_type = Relationship.RelationshipType,
			relationship_status = Relationship.RelationshipStatus
		) %>%
		clean_lei_relationship_dates()

}




clean_lei_relationship_dates <- function(raw_lei_relationships){
	#' Clean LEI relationships
	#'
	#' Find the relationship period dates and convert them into a useful format

	base_lei_rel <- raw_lei_relationships %>%
		dplyr::mutate(row_id = dplyr::row_number())

	p1 <- base_lei_rel %>%
		dplyr::select(row_id,
									start_date = Relationship.Period.1.startDate,
									end_date = Relationship.Period.1.endDate,
									period_type = Relationship.Period.1.periodType)

	p2 <- base_lei_rel %>%
		dplyr::select(row_id,
									start_date = Relationship.Period.2.startDate,
									end_date = Relationship.Period.2.endDate,
									period_type = Relationship.Period.2.periodType)

	p3 <- base_lei_rel %>%
		dplyr::select(row_id,
									start_date = Relationship.Period.3.startDate,
									end_date = Relationship.Period.3.endDate,
									period_type = Relationship.Period.3.periodType)

	p4 <- base_lei_rel %>%
		dplyr::select(row_id,
									start_date = Relationship.Period.4.startDate,
									end_date = Relationship.Period.4.endDate,
									Relationship.Period.4.periodType)

	p5 <- base_lei_rel %>%
		dplyr::select(row_id,
									start_date = Relationship.Period.5.startDate,
									end_date = Relationship.Period.5.endDate,
									period_type = Relationship.Period.5.periodType)

	rel_dates <- dplyr::bind_rows(p1,p2,p3,p4,p5) %>%
		dplyr::filter(period_type == "RELATIONSHIP_PERIOD") %>%
		dplyr::select(row_id,
									start_date,
									end_date)

	if(nrow(rel_dates) > dplyr::n_distinct(rel_dates$row_id)){
		rel_dates <- rel_dates %>%
			dplyr::group_by(row_id) %>%
			dplyr::summarise(start_date = min(start_date),
											 end_date = max(end_date))
	}

	clean_rel_dates <- rel_dates %>%
		dplyr::mutate(
			start_date = parse_lei_ts(start_date),
			end_date = parse_lei_ts(end_date)
		)

	base_lei_rel %>%
		dplyr::left_join(clean_rel_dates, by = "row_id") %>%
		dplyr::select(
			start_node_id,
			end_node_id,
			relationship_type,
			relationship_status,
			relationship_start_date = start_date,
			relationship_end_date = end_date
		)

}
