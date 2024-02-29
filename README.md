# eerstelijnSchelde

## Achtergrond

Deltares maakt elk jaar een eerstelijnsrapportage voor Rijkswaterstaat over de monitoring in de Westerschelde en de monding daarvan. dit is een beschrijving van alle metingen, statistiek, trends, ontbrekende waarden, kaarten en dergelijke. De rapportage wordt gebruikt om een eerste indruk te krijgen van de toestand in de Westerschelde. 

## Opzet

Het rapport wordt opgebouwd met R scripts en vormgegeven met bookdown (). De code hiervoor staat in deze repository. De laatste versie van het rapport kan worden bekeken op https://scheldemonitor.github.io/Eerstelijnsrapportage/. 

Voor de rapportage worden gegevens opgehaald uit de Scheldemonitor (https://www.scheldemonitor.org) via een WFS service. Daarna worden bewerkingen op deze data gedaan, grafieken en tabellen gemaakt, en kaartjes gemaakt. De gebruikte software voor de opzet van de rapportage is R met de packages Rmarkdown and Bookdown.

## Betrokkenen

Deltares: Willem Stolte, Jelle Rienstra, Bob van Rongen, Frank Kleissen, Marcel Taal. Bij eerdere rapportages hebben nog vele anderen meegewerkt.

Contact via https://github.com/wstolte
Suggesties voor verbeteringen: https://github.com/scheldemonitor/Eerstelijnsrapportage/issues
Rijkswaterstaat: Albert Mulder
