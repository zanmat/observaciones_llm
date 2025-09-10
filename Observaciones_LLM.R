

library(ellmer)
library(robotoolbox)
library(dplyr)
library(labelled)
library(stringr)

kobo_setup(url = "https://kobo.unhcr.org",
           token = Sys.getenv('KOBO_API_KEY'))

RUDAH_2025 <- kobo_submissions("aEwieSotrtjeaUn6RLDCeo")

main_2025 <- RUDAH_2025$main |> 
  filter(test_real == "registro_real") |> 
  filter(kit_orientacion == "observacion") |> 
  select(today, region, provincia, punto_observacion, 
         #ppl_total, nacionalidad_observada, 
         comentarios_ob) |> 
  filter(!is.na(comentarios_ob)) |> 
  mutate(comentarios_ob = str_replace_all(comentarios_ob, "PNP", "Policía Nacional")) |> 
  mutate(comentarios_ob = str_replace_all(comentarios_ob, "PDF", "Persona desplazada")) |> 
  mutate(comentarios_ob = str_replace_all(comentarios_ob, "NNA", "Niña, Niño o Adolescente"))
  

  #mutate(nacionalidad_observada = if_else(is.na(nacionalidad_observada), "", nacionalidad_observada))

names_lug_ori <- c('region', 'provincia', 'punto_observacion')
main_2025[,names_lug_ori] <- lapply(main_2025[,names_lug_ori] , to_factor)

main_2025 <- main_2025 |> 
  # mutate(observaciones = paste0("Fecha: ", today, 
  #                               " Región: ", region, 
  #                               " Provincia: ", provincia, 
  #                               " Punto de observación: ", punto_observacion,
  #                               " Observaciones: ", comentarios_ob)) |> 
  select(region, comentarios_ob) |> 
  arrange(region) |> 
  group_by(region) |>
  summarise(envíos = n())

  summarise(comentarios_ob = paste(comentarios_ob, collapse = " \n ")) |> 
  ungroup() |> 
  mutate(n_char = nchar(comentarios_ob))

options(ellmer_timeout_s = 1000)

chat <- chat_ollama(system_prompt="You are a Spanish-speaking humanitarian worker 
                    with particular focus on international protection.", model = "gemma3:12b")

type_observaciones <- type_object(
  "Resumen",
  resumen = type_string("Este texto consiste de observaciones de actores humanitarios en zonas fronterizas en Perú.
                        Brinda un resumen en 100 palabras o menos de los puntos mencionados en el texto. 
                        Pon particular atención a temas de protección internacional."),
  region = type_string("En cuál región de Perú se hicieron las observaciones? 
                       Menciona solo la región."),
  )

prompts_list <- as.list(main_2025$observaciones)

df <- parallel_chat_structured(chat, prompts_list, type = type_observaciones)

#openxlsx::write.xlsx(df, "observaciones.xlsx")


