
# Benoetigte Packete installieren
install.packages("RODBC")
install.packages("data.table") # fuer die SQL-Freunde unter uns

# Pakete inkludieren
library(RODBC)
library(data.table)

# Verbindung aufbauen
connection <- odbcDriverConnect('driver={SQL Server};server=sqlrem;database=rem_privera_flatfox;trusted_connection=true')

# Tabelle holen
mietzinse <- sqlQuery(connection, "select mz.GueltigAb ,mz.BetragNetto, abr.KantonCD, 
		                  abr.Ort,
                      abr.Bezeichnung1, 
                      art.Bez,
                      art.ObjektKategorieCD,
                      o.AnzahlZimmerCD
                      from Mietzins mz
                      join Objekt o on mz.ObjektOID = o.OID
                      join Haus h on o.HausOID = h.OID
                      join AbrechEinheit abr on abr.OID = h.AbrechEinheitOID
                      join ObjektArt art on art.OID = o.ObjektArtOID
                      where GueltigAb > \'2000-01-01\'
                      and GueltigAb < \'2017-01-01\'
                      order by Ort")

# In data.table verwandeln, damit wir im bekannten Terrain sind
mietzinse_table <- na.omit(data.table(mietzinse))

# Summry ausgeben
count(mietzinse_table)
summary(mietzinse_table)


# Jahr-Spalte einfuehren
mietzinse_table$Jahr <- format(mietzinse_table$GueltigAb, "%Y")


## DT[i, j, by]
##   R:      i                 j        by
## SQL:  where   select | update | group by
## (Take DT, subset rows using i, then calculate j, grouped by by.)

# Sortieren, Filtern, Berechnen und Gruppieren
mietzins_aggregated <- mietzinse_table[order(Jahr, AnzahlZimmerCD == 3.5), mean(BetragNetto), by = Jahr]

# Default-Plot erstellen
plot(mietzins_aggregated)

# Verbindung wieder schliessen
close(connection)