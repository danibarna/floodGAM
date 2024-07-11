Hvordan henter jeg data med lescon_var
================

Du kan hente data fra NVE sine databaser med (i) [bruk av
API](https://hydapi.nve.no/UserDocumentation/#gettingstarted) eller (ii)
bruk av lescon_var (internt system). Den andre er betyedelig raskere.

## Grunnleggende lescon_var detaljer

Kodelinjen som kreves for å bruke lescon_var til å hente data fra en
enkelt stasjon ser slik ut:

**lescon_var -b 0 -f timevalue 35 1 200 0 1001 1 \> 100200.txt**

- **b 0** betyr at vi får den opprinnelige tidsoppløsningen.

- **f timevalue** betyr at vi får tiden i en kolonne og verdiene i en
  annen.

- **35** er arkivet dataene leses fra.

- **1 200 0 1001 1** er en kode for stasjonen:

  - ‘1’ er reginenummer
  - ‘200’ er hovednummer
  - ‘0’ er punktnummer og kan alltid være null
  - ‘1001’ er vannføring og kan alltid være ‘1001’
  - ‘1’ er versjonsnummer

- **100200.txt** er navnet på datafilen. Vi velger denne.

For å hente flere detaljer om lescon_var, åpne smarTTY og skriv
**lescon_var** i kommandolinjen.

## Bruke R til å lage en lescon_var kjørbar fil

En lescon_var kjørbar fil er en .txt-fil som vi kan laste opp til en nve
server og gjøre kjørbar på kommandolinjen. Hver linje i .txt-filen er en
lescon_var-kommando som henter data for en enkelt stasjon. Siden vi
trenger data fra flere hundre stasjoner, er det praktisk å bruke en
automatisert metode (e.g. bash) for å lage .txt-filen. Denne kan vi
gjøre med R.

Skriptet for å gjøre dette finnes [her](link) og kjørbar filen selv er
lagret [her](link).

## Kjør lescon_var

Installer smarTTY fra firmaportalen. Åpne det.

Klikk “create a new SSH connection”. Host: l-ts02. Username: ditt NVE
brukernavn. Password: ditt NVE passord.

Klikk “start with a regular terminal”. Forbindelsen skal nå være åpen.

Skriv **kinit**. Trykk enter. Skriv ditt NVE passord. Trykk enter.

Naviger til ClimDesign-mappen (eller en annen mappe du ønsker) ved hjelp
av kommandolinjen i smarTTY. For å gjøre dette, skriv **cd
/path_to_your_directory** i smarTTY-vinduet og trykk enter.

Last opp lescon_var-kjørbarfilen du skrev med R. Klikk på “SCP”-fanen i
smarTTY-grensesnittet. Klikk “upload a file”. Last opp
my_lescon_var_commands.txt. Den skal nå være i ClimDesign-mappen.

Gjør my_lescon_var_commands.txt kjørbar på kommandolinjen. Skriv **chmod
u+x my_lescon_var_commands.txt** og trykk enter.

Kjør my_lescon_var_commands.txt. Skriv **./my_lescon_var_commands.txt**
og trykk enter.

Dataene vil bli lastet ned til ClimDesign-katalogen. For å få det på din
lokale maskin, klikk på “SCP”-fanen i smarTTY og klikk “download
directory”. Last det ned et sted du har tilgang til på din lokale
maskin. Jeg pleier å lage en ny mappe på C-stasjonen min (ikke veldig
elegant, men enkelt). Bruk “on-the-fly TAR”-alternativet for
nedlastingen.

Hvis du noen gang trenger å avslutte en hengende prosess, skriv
**ctrl-Z**.

Hvis du noen gang trenger å slette en fil ved hjelp av SSH, skriv **rm
myFile\*.txt**. Dette vil matche alle filer som starter med “myFile” og
slutter med “.txt” og slette dem.
