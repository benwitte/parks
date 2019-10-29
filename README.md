# New York City Parks

### Note: All Analysis Conducted by New York City Council. This is a copy of original repository, found here: https://github.com/NewYorkCityCouncil/Parks_PA

## Objective

Maps of NYC parks amenities, events, and funding. Events data updates daily via a cron job on the data team's server.

### Documents

- `code/`
  - `events.R`: Pull events data from DPR feed and create map
  - `parks_funding.R`: Read funding data and create map
  - `parks.R`: Create amenities map
  - `util.R`: Various utility functions (*e.g* add search to maps)
- `data/`: where the various data sources should be placed
- `results/`: Output files shown on the website
  - `data_files/`: JSON files of clean data used to generate tables
  - `num_events.json`: JSON file of events, updated daily
- `update_events.sh`: script run daily to update events map

## Development

Because this repo is updated daily via cron job and because two branches are involved (`master` and `gh-pages`), changing this repo is liable to break the update. **Do not push to this repo if you're unsure of what's happening.**

To prevent conflicts, follow this workflow:

1. Pull both `master` and `gh-pages`.
2. Commit changes to `master`.
3. Rebase changes on to `gh-pages`.
4. Push both master and `gh-pages`.
5. SSH into server, cd to `/home/nick/Parks_PA` (you'll need to either be Nick or use `sudo`).
6. Pull changes to both `master` and `gh-pages`
