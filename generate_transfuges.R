# chasse aux transfuges depuis l'an passé
# un transfuge est quelqu un qui donne a un seul parti cette annee 
# et un seul l an passé
# mais pas le même 
last2years_1partimax <- wrangled_file %>% 
  filter(annee_financiere>= max(annee_financiere)- 1) %>%
  group_by(nom_prenom, code_postal, annee_financiere) %>%
  filter(n() == 1) %>% 
  ungroup()

transfuges <-  last2years_1partimax %>% 
  filter(annee_financiere== max(annee_financiere)) %>%
  select(nom_prenom, entite_politique, code_postal, annee_financiere) %>%
  inner_join(# deja vu quelque part l an passé
    last2years_1partimax %>% 
      filter(annee_financiere == (max(annee_financiere)-1 )) %>% 
      select(nom_prenom,  code_postal) %>% 
      distinct() 
  ) %>% #mais pas chez nous
  anti_join(
    last2years_1partimax %>% 
      filter(annee_financiere == (max(annee_financiere))-1) %>% 
      select(nom_prenom,  code_postal,entite_politique) %>% 
      distinct() 
  )

transfuges %>% 
  left_join(
    last2years_1partimax %>% 
      filter(annee_financiere == max(annee_financiere)-1 ) %>%
      select(nom_prenom, code_postal, parti_origine = entite_politique)
  ) %>% 
  count(entite_politique, parti_origine)
