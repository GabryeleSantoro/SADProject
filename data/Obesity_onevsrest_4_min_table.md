# Obesity Dataset - Feature Reference

| Colonna                        | Descrizione                                             | Tipi / Valori possibili                                                |
| ------------------------------ | ------------------------------------------------------- | ---------------------------------------------------------------------- |
| Gender                         | Genere del soggetto                                     | 0 = Femmina, 1 = Maschio                                               |
| AGE                            | Età del soggetto in anni                                | Valori reali continui (es. 15-52)                                      |
| Height                         | Altezza del soggetto in metri                           | Valori reali continui (es. 1.5-1.98)                                   |
| Weight                         | Peso del soggetto in chilogrammi                        | Valori reali continui (es. 75-125)                                     |
| family_history_with_overweight | Presenza di casi di sovrappeso in famiglia              | 0 = No, 1 = Sì                                                         |
| FAVC                           | Consumo frequente di cibi ad alto contenuto calorico    | 0 = No, 1 = Sì                                                         |
| FCVC                           | Frequenza di consumo di verdure                         | 1 = Mai, 2 = A volte, 3 = Sempre                                       |
| NCP                            | Numero di pasti principali al giorno                    | 1 = 1 pasto, 2 = 2 pasti, 3 = 3 pasti, 4 = 4+ pasti                    |
| CAEC                           | Consumo di cibo tra i pasti                             | 0 = No, 1 = A volte, 2 = Frequentemente, 3 = Sempre                    |
| SMOKE                          | Abitudine al fumo                                       | 0 = No, 1 = Sì                                                         |
| CH2O                           | Quantità di acqua bevuta al giorno (litri)              | 1 = <1L, 2 = 1-2L, 3 = >2L                                             |
| SCC                            | Monitoraggio del consumo calorico                       | 0 = No, 1 = Sì                                                         |
| FAF                            | Frequenza di attività fisica settimanale                | 0 = Nessuna, 1 = 1-2 giorni, 2 = 2-4 giorni, 3 = 4-5 giorni            |
| TUE                            | Tempo giornaliero trascorso con dispositivi tecnologici | 0 = 0-2 ore, 1 = 3-5 ore, 2 = >5 ore                                   |
| CALC                           | Consumo di alcol                                        | 0 = Mai, 1 = A volte, 2 = Frequentemente, 3 = Sempre                   |
| MTRANS                         | Mezzo di trasporto principale                           | 0 = Automobile, 1 = Moto, 2 = Bicicletta, 3 = Trasporto pubblico/Piedi |
| class                          | Classe target (livello di obesità)                      | 1 = Obeso (dataset one-vs-rest)                                        |
