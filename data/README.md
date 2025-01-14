
<!-- <h1 align="center">floodGAM datasets</h1> -->

## File structure

- `raw-data` – HYDRA II database commands, original tables from reports,
  and other raw data.

- `processed-data` – Any data loaded/manipulated/changed/saved with code
  from the `code` folders and ready for analysis. Any files located in
  here are based on the raw data and can be re-created with the scripts
  in the [1-data-creation](/code/scripts/1-data-creation) folder.

- `how-to-guides` – how to use the database commands in `raw-data`.

The `cleaned-data` folder can be used to save intermediate datafiles
when processing the (very large) streamflow record files. It is mostly
empty in the online version of the repository (files too large for
upload).
