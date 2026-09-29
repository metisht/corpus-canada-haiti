# Builds the curated engagement layer from retained official evidence.
# Inputs: data/corpus.csv and the explicit engagement definitions below.
# Outputs: data/engagements.csv, data/engagement_documents.csv, and
# data/engagement_instruments.csv. The script never changes corpus decisions.

corpus <- readr::read_csv("data/corpus.csv", show_col_types = FALSE)

engagements <- data.frame(
  engagement_id = c(
    "eng_escort", "eng_cauldron", "eng_forward_action", "eng_cadence",
    "eng_dialogue", "eng_unmih_advance_team_1994", "eng_police_training_initiative_1994",
    "eng_pivot", "eng_standard_unmih", "eng_standard_unsmih", "eng_stable",
    "eng_constable", "eng_compliment", "eng_haiti_1999", "eng_haiti_flood_airlift_2004",
    "eng_earthquake_response_2010", "eng_support_package_2022",
    "eng_haiti_sema_regime_2022", "eng_haiti_un_sanctions_regime_2022",
    "eng_sanctions_2022_11_04", "eng_sanctions_2022_11_19",
    "eng_sanctions_2022_12_05", "eng_sanctions_2022_12_20",
    "eng_sanctions_2023_01_13", "eng_caricom_partnership_2023",
    "eng_ministerial_coordination_2023", "eng_sanctions_2023_09_21",
    "eng_security_funding_2024", "eng_sanctions_2024_06_21",
    "eng_police_support_2024_2025", "eng_sanctions_2025_03_21",
    "eng_security_assistance_2025"
  ),
  label_en = c(
    "Operation ESCORT", "Operation CAULDRON", "Operation FORWARD ACTION", "Operation CADENCE",
    "Operation DIALOGUE", "Canadian participation in the UNMIH advance team", "Canadian Haitian police-training initiative",
    "Operation PIVOT", "Operation STANDARD — UNMIH", "Operation STANDARD — UNSMIH", "Operation STABLE",
    "Operation CONSTABLE", "Operation COMPLIMENT", "Haiti 1999 assistance flight", "2004 Haiti flood-relief airlift",
    "2010 earthquake response and reconstruction support", "$50.4 million Haiti support package",
    "Special Economic Measures (Haiti) regime", "Implementation of UN Haiti sanctions",
    "Sanctions designation — 4 November 2022", "Sanctions designation — 19 November 2022",
    "Sanctions designation — 5 December 2022", "Sanctions designation — 20 December 2022",
    "Sanctions designation — 13 January 2023", "Canada–CARICOM Strategic Partnership",
    "Ministerial coordination and security support", "Sanctions designation — 21 September 2023",
    "$123 million Haiti security package", "Sanctions designation — 21 June 2024",
    "Canadian international police support, 2024–2025", "Sanctions designation — 21 March 2025",
    "$60 million security-assistance commitment"
  ),
  label_fr = c(
    "Opération ESCORT", "Opération CAULDRON", "Opération FORWARD ACTION", "Opération CADENCE",
    "Opération DIALOGUE", "Participation canadienne à l’équipe préparatoire de la MINUHA", "Initiative canadienne de formation policière haïtienne",
    "Opération PIVOT", "Opération STANDARD — MINUHA", "Opération STANDARD — MANUH", "Opération STABLE",
    "Opération CONSTABLE", "Opération COMPLIMENT", "Vol d’aide Haïti 1999", "Pont aérien de secours après les inondations de 2004",
    "Réponse au séisme et soutien à la reconstruction de 2010", "Programme de soutien à Haïti de 50,4 millions de dollars",
    "Régime de mesures économiques spéciales visant Haïti", "Mise en œuvre des sanctions de l’ONU visant Haïti",
    "Désignation de sanctions — 4 novembre 2022", "Désignation de sanctions — 19 novembre 2022",
    "Désignation de sanctions — 5 décembre 2022", "Désignation de sanctions — 20 décembre 2022",
    "Désignation de sanctions — 13 janvier 2023", "Partenariat stratégique Canada–CARICOM",
    "Coordination ministérielle et soutien à la sécurité", "Désignation de sanctions — 21 septembre 2023",
    "Programme de sécurité en Haïti de 123 millions de dollars", "Désignation de sanctions — 21 juin 2024",
    "Soutien policier international canadien, 2024–2025", "Désignation de sanctions — 21 mars 2025",
    "Engagement de 60 millions de dollars pour l’aide à la sécurité"
  ),
  start_date = c(
    "1991-11-01", "1993-10-01", "1993-10-18", "1994-09-01", "1994-09-01",
    "1994-09-01", "1994-01-25", "1995-04-01", "1996-04-15", "1996-06-30",
    "1996-10-01", "1997-07-30", "1998-01-01", "1999-01-12", "2004-09-22",
    "2010-01-12", "2022-01-21", "2022-11-03", "2022-11-10", "2022-11-04",
    "2022-11-19", "2022-12-05", "2022-12-20", "2023-01-13", "2023-07-01",
    "2023-06-15", "2023-09-21", "2024-02-22", "2024-06-21", "2024-04-01",
    "2025-03-21", "2025-09-25"
  ),
  end_date = c(
    "1992-03-30", "1993-10-31", "1994-09-29", "1994-09-30", "1994-10-12",
    "1994-10-31", "", "1996-04-15", "1996-06-30", "1996-09-30", "1997-07-31",
    "1997-11-30", "2000-03-16", "1999-01-12", "2004-12-20", "", "", "", "",
    "", "", "", "", "", "", "", "", "", "", "2025-03-31", "", ""
  ),
  date_precision = c(
    "day", "month", "day", "month", "month", "month", "documented announcement date",
    rep("day", 25)
  ),
  lead_institution = c(
    rep("National Defence", 6), "Global Affairs Canada", rep("National Defence", 8),
    "Government of Canada", "Global Affairs Canada", "Global Affairs Canada", "Global Affairs Canada",
    rep("Global Affairs Canada", 10), "Royal Canadian Mounted Police", "Global Affairs Canada", "Global Affairs Canada"
  ),
  engagement_status = c(rep("completed", 17), "active", "active", rep("completed", 13)),
  geographic_scope = rep("Haiti", 32),
  description_en = c(
    "Contingency planning and military capacity for a possible evacuation of Canadian nationals; the evacuation was not activated.",
    "Canada’s October 1993 engineering contribution to the first attempted UNMIH deployment was stood down after the mission could not land.",
    "Canadian warships enforced the United Nations oil and arms embargo through maritime interception operations off Haiti.",
    "Canadian military observers monitored traffic across the Dominican Republic–Haiti border and reported on embargo compliance.",
    "Canadian naval and airborne forces prepared to evacuate Canadian personnel in September 1994; the extraction was not required.",
    "A Canadian-led UNMIH advance team arrived in September and October 1994; Canadian civilian police participated alongside military officers.",
    "Canada proposed a UN-supported program to train a Haitian police force and agreed with President Aristide to develop its terms.",
    "About 500 Canadian personnel provided UNMIH aviation, engineering, transport, communications, security and logistical support.",
    "A predominantly army contingent provided patrols, security, engineering, helicopters, logistics and headquarters support during UNMIH’s final phase.",
    "The Canadian contingent continued under UNSMIH with patrol, engineering, aviation and logistical support for civilian and Haitian police.",
    "Canada provided military personnel and civilian police for patrols, police training, peace-support duties and humanitarian assistance under UNSMIH.",
    "Canada contributed military personnel and civilian police to UNTMIH for patrol, protection, police professionalization and humanitarian work.",
    "Canada supplied armoured vehicles, driving instructors and maintenance personnel to support MIPONUH’s police mission.",
    "A Canadian military flight delivered official assistance to Haiti on 12 January 1999.",
    "Canadian military aircraft delivered relief supplies in four documented deployments after the 2004 floods and hurricanes.",
    "Canada combined emergency response, reconstruction funding, corrections deployments and security and justice support after the 2010 earthquake.",
    "Canada announced $50.4 million for projects addressing security, health, education and food security in Haiti.",
    "Canada established a sanctions regime under the Special Economic Measures Act in response to the crisis in Haiti.",
    "Canada implemented United Nations Security Council sanctions concerning Haiti under the United Nations Act.",
    rep("Canada imposed a distinct round of targeted sanctions under the Haiti regulations.", 5),
    "Canada and CARICOM established a framework covering regional security and coordinated action concerning Haiti.",
    "Canada convened international partners and announced security-related support concerning Haiti.",
    "Canada imposed a distinct round of targeted sanctions under the Haiti regulations.",
    "Canada announced $123 million for Haitian security, police capacity and related support.",
    "Canada imposed a distinct round of targeted sanctions under the Haiti regulations.",
    "Canadian police and civilian experts advised and trained the Haitian National Police through the international policing program.",
    "Canada imposed a distinct round of targeted sanctions under the Haiti regulations.",
    "Canada announced security assistance including support for the multinational mission and a regional maritime-security initiative."
  ),
  description_fr = c(
    "Planification et capacités militaires en vue d’une éventuelle évacuation de ressortissants canadiens; l’évacuation n’a pas été déclenchée.",
    "La contribution canadienne de génie à la première tentative de déploiement de la MINUHA en octobre 1993 a été retirée après l’impossibilité de débarquer.",
    "Des navires canadiens ont appliqué l’embargo des Nations Unies sur le pétrole et les armes par des interceptions maritimes au large d’Haïti.",
    "Des observateurs militaires canadiens ont surveillé la frontière entre la République dominicaine et Haïti et rendu compte du respect de l’embargo.",
    "Les forces navales et aéroportées canadiennes ont préparé une évacuation en septembre 1994; l’extraction n’a pas été nécessaire.",
    "Une équipe préparatoire de la MINUHA dirigée par un Canadien est arrivée en septembre et octobre 1994; des policiers civils canadiens y participaient.",
    "Le Canada a proposé un programme soutenu par l’ONU pour former une police haïtienne et a convenu avec le président Aristide d’en préciser les modalités.",
    "Environ 500 militaires canadiens ont fourni à la MINUHA des capacités aériennes, de génie, de transport, de communication, de sécurité et de logistique.",
    "Un contingent principalement terrestre a assuré patrouilles, sécurité, génie, aviation, logistique et soutien d’état-major pendant la dernière phase de la MINUHA.",
    "Le contingent canadien a poursuivi son action sous la MANUH avec des capacités de patrouille, de génie, d’aviation et de logistique au profit des polices civile et haïtienne.",
    "Le Canada a fourni des militaires et des policiers civils pour les patrouilles, la formation policière, le soutien de la paix et l’aide humanitaire sous la MANUH.",
    "Le Canada a fourni des militaires et des policiers civils à la MITNUH pour la patrouille, la protection, la professionnalisation policière et l’aide humanitaire.",
    "Le Canada a fourni des véhicules blindés, des instructeurs de conduite et des techniciens pour soutenir la mission policière de la MIPONUH.",
    "Un vol militaire canadien a livré une aide officielle à Haïti le 12 janvier 1999.",
    "Des aéronefs militaires canadiens ont livré des secours lors de quatre déploiements documentés après les inondations et ouragans de 2004.",
    "Après le séisme de 2010, le Canada a combiné intervention d’urgence, financement de la reconstruction, déploiement correctionnel et appui à la sécurité et à la justice.",
    "Le Canada a annoncé 50,4 millions de dollars pour des projets touchant la sécurité, la santé, l’éducation et la sécurité alimentaire en Haïti.",
    "Le Canada a établi un régime de sanctions en vertu de la Loi sur les mesures économiques spéciales en réponse à la crise en Haïti.",
    "Le Canada a mis en œuvre les sanctions du Conseil de sécurité des Nations Unies visant Haïti en vertu de la Loi sur les Nations Unies.",
    rep("Le Canada a imposé une série distincte de sanctions ciblées en vertu du règlement visant Haïti.", 5),
    "Le Canada et la CARICOM ont établi un cadre portant notamment sur la sécurité régionale et l’action coordonnée concernant Haïti.",
    "Le Canada a réuni des partenaires internationaux et annoncé un soutien lié à la sécurité en Haïti.",
    "Le Canada a imposé une série distincte de sanctions ciblées en vertu du règlement visant Haïti.",
    "Le Canada a annoncé 123 millions de dollars pour la sécurité en Haïti, les capacités policières et des mesures connexes.",
    "Le Canada a imposé une série distincte de sanctions ciblées en vertu du règlement visant Haïti.",
    "Des policiers et experts civils canadiens ont conseillé et formé la Police nationale d’Haïti dans le cadre du programme de police internationale.",
    "Le Canada a imposé une série distincte de sanctions ciblées en vertu du règlement visant Haïti.",
    "Le Canada a annoncé une aide à la sécurité comprenant un soutien à la mission multinationale et une initiative régionale de sécurité maritime."
  ),
  coding_status = "confirmed",
  coding_notes = "Curated from retained official evidence; document identity is not used as engagement identity.",
  stringsAsFactors = FALSE
)

document_map <- list(
  eng_escort = "doc_74d86079e84f4956", eng_cauldron = "doc_9383c9876e29ccab",
  eng_forward_action = "doc_33e8d977deaec6b4", eng_cadence = "doc_dce04c4a72239704",
  eng_dialogue = "doc_40864f7051423269", eng_unmih_advance_team_1994 = "doc_53c8e53f372060e4",
  eng_police_training_initiative_1994 = "doc_19a800832566b15e", eng_pivot = "doc_53c8e53f372060e4",
  eng_standard_unmih = "doc_0600194fddc50308", eng_standard_unsmih = "doc_f43c7a0af065dcef",
  eng_stable = "doc_89c03871212e57d1", eng_constable = "doc_d3a0bd12e53725e7",
  eng_compliment = "doc_a594f0dbf555ad74", eng_haiti_1999 = "doc_6be9585faccc4f7a",
  eng_haiti_flood_airlift_2004 = c("doc_1d1eaafa5cce2a2d", "doc_f52e050092a968d9", "doc_20635e8af9638f0c", "doc_472b62aa5d6660ea"),
  eng_earthquake_response_2010 = c("doc_7cecb5ad0657869c", "doc_c66aba07877243fe", "doc_5c4fd786da08dd80", "doc_79aec294241bb2bc"),
  eng_support_package_2022 = "doc_01014bd134ebab17",
  eng_haiti_sema_regime_2022 = c("doc_50bc0e9662ed4a7f", "doc_37f1777035ca16f7"),
  eng_haiti_un_sanctions_regime_2022 = c("doc_df22bbd080fc675e", "doc_98ab02dc33b997a7"),
  eng_sanctions_2022_11_04 = c("doc_d4ba178c32fff813", "doc_8beb617c16e0a8f8"),
  eng_sanctions_2022_11_19 = "doc_a440cfd066679311", eng_sanctions_2022_12_05 = "doc_ed6882b9667e1c7a",
  eng_sanctions_2022_12_20 = "doc_69d48fb2875b3537", eng_sanctions_2023_01_13 = "doc_446ed6e0029a3c15",
  eng_caricom_partnership_2023 = "doc_e86d697db083afb8", eng_ministerial_coordination_2023 = "doc_951a3585c8cc0883",
  eng_sanctions_2023_09_21 = "doc_efb13f470393d5e5", eng_security_funding_2024 = "doc_5e34174ad5474dde",
  eng_sanctions_2024_06_21 = c("doc_2d57be6a22f9f43b", "doc_6c20483be6a4fdae"),
  eng_police_support_2024_2025 = "doc_d2d9aac6534a54ce", eng_sanctions_2025_03_21 = c("doc_f5a69ac74f7e5499", "doc_83e9a22aa6a888b4"),
  eng_security_assistance_2025 = "doc_f58312cd85500bb9"
)

instrument_map <- list(
  eng_escort = "military_operation", eng_cauldron = c("military_operation", "institutional_support"),
  eng_forward_action = c("military_operation", "sanctions_regulatory"), eng_cadence = c("military_operation", "sanctions_regulatory"),
  eng_dialogue = "military_operation", eng_unmih_advance_team_1994 = c("military_operation", "police_deployment"),
  eng_police_training_initiative_1994 = c("police_training", "diplomatic_multilateral"),
  eng_pivot = c("military_operation", "institutional_support", "police_deployment", "police_training"),
  eng_standard_unmih = c("military_operation", "institutional_support"), eng_standard_unsmih = c("military_operation", "institutional_support", "police_training"),
  eng_stable = c("military_operation", "police_deployment", "police_training", "financial_humanitarian"),
  eng_constable = c("military_operation", "police_deployment", "police_training", "financial_humanitarian"),
  eng_compliment = c("military_operation", "police_training"), eng_haiti_1999 = c("military_operation", "financial_humanitarian"),
  eng_haiti_flood_airlift_2004 = c("military_operation", "financial_humanitarian"),
  eng_earthquake_response_2010 = c("financial_humanitarian", "institutional_support", "police_deployment"),
  eng_support_package_2022 = c("financial_humanitarian", "development_program", "institutional_support"),
  eng_haiti_sema_regime_2022 = "sanctions_regulatory", eng_haiti_un_sanctions_regime_2022 = c("sanctions_regulatory", "diplomatic_multilateral"),
  eng_sanctions_2022_11_04 = "sanctions_regulatory", eng_sanctions_2022_11_19 = "sanctions_regulatory",
  eng_sanctions_2022_12_05 = "sanctions_regulatory", eng_sanctions_2022_12_20 = "sanctions_regulatory",
  eng_sanctions_2023_01_13 = "sanctions_regulatory", eng_caricom_partnership_2023 = "diplomatic_multilateral",
  eng_ministerial_coordination_2023 = c("diplomatic_multilateral", "financial_humanitarian"),
  eng_sanctions_2023_09_21 = "sanctions_regulatory", eng_security_funding_2024 = c("financial_humanitarian", "police_training", "institutional_support"),
  eng_sanctions_2024_06_21 = "sanctions_regulatory", eng_police_support_2024_2025 = c("police_deployment", "police_training", "institutional_support"),
  eng_sanctions_2025_03_21 = "sanctions_regulatory", eng_security_assistance_2025 = c("financial_humanitarian", "diplomatic_multilateral")
)

retained_ids <- unique(corpus$document_id[corpus$review_status == "retain"])
stopifnot(!anyDuplicated(engagements$engagement_id))
stopifnot(all(unlist(document_map, use.names = FALSE) %in% retained_ids))

engagement_documents <- do.call(rbind, lapply(names(document_map), function(engagement_id) {
  ids <- document_map[[engagement_id]]
  rows <- corpus[match(ids, corpus$document_id), , drop = FALSE]
  data.frame(
    engagement_id = engagement_id,
    document_id = ids,
    evidence_role = ifelse(seq_along(ids) == 1L, "primary_official_evidence", "corroborating_official_evidence"),
    evidence_excerpt = rows$relevance_excerpt,
    coding_status = "confirmed",
    coding_notes = "Linked only where the retained document explicitly evidences this engagement.",
    stringsAsFactors = FALSE
  )
}))

# The retained Hansard contains many unrelated exchanges. This exact passage,
# already stored in the official record, is the evidence for the distinct 1994
# police-training initiative; PIVOT keeps its separately documented 1995–1996
# dates and evidence.
police_training_1994_excerpt <- paste(
  "Does the Canadian government intend, as it has repeatedly said, to help train",
  "Haitian police forces whose mandate it would be to restore the country’s",
  "democratic institutions? Did the minister make any formal commitments in this",
  "respect in the course of his talks with Mr. Aristide? Hon. André Ouellet",
  "(Minister of Foreign Affairs): Mr. Speaker, the Prime Minister discussed this",
  "matter when he met with the Secretary General of the United Nations in Paris.",
  "We feel it would be a good idea to organize under the auspices of the United",
  "Nations this kind of training program for police officers from countries under",
  "military dictatorship. This proposal which was put forward by Canada could",
  "apply first to Haiti. [...] During the course of our discussions with President",
  "Aristide yesterday, we obtained his support for such an initiative."
)
engagement_documents$evidence_excerpt[
  engagement_documents$engagement_id == "eng_police_training_initiative_1994"
] <- police_training_1994_excerpt

engagement_instruments <- do.call(rbind, lapply(names(instrument_map), function(engagement_id) {
  data.frame(engagement_id = engagement_id, instrument_code = instrument_map[[engagement_id]], stringsAsFactors = FALSE)
}))

allowed_instruments <- c("military_operation", "police_training", "police_deployment", "institutional_support", "diplomatic_multilateral", "sanctions_regulatory", "development_program", "financial_humanitarian")
stopifnot(all(engagement_instruments$instrument_code %in% allowed_instruments))
stopifnot(all(engagement_documents$engagement_id %in% engagements$engagement_id))
stopifnot(all(engagement_instruments$engagement_id %in% engagements$engagement_id))

readr::write_csv(engagements, "data/engagements.csv", na = "")
readr::write_csv(engagement_documents, "data/engagement_documents.csv", na = "")
readr::write_csv(engagement_instruments, "data/engagement_instruments.csv", na = "")
