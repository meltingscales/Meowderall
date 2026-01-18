# Meowderall

to help shelters track cat medication schedules digitally

SPA web app

## stack

- elm
- spa web app
- only local storage. make sure people know there's no backend, it's local only
- option to download pdf and csv

## prompt for ai

1. (optional) read ./*.csv
2. primary interaction is via iPad - tapping/typing on touch screen
3. this app will help animal shelters manage cat medication schedules.
4. multiple "admins" exist that enter 4-digit PIN codes to modify AM/PM initial fields. all others are world-rw.
5. SPA web app. use elm lang please.
6. use a `justfile` w/ `just` for storing useful commands :)
7. extremely simple deploy to gcp. please copy ~/Git/drakonix.systems in terms of justfile structure/deploy steps w/ gcloud cli.

### addtl' notes.

use this repo (see ~/Git) as an example of website feel, FAQs, etc.

https://github.com/meltingscales/CAREShelterDonationDataAggregation

