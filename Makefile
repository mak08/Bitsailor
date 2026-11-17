# Static datasets for the router. Fedora 44+ / GNU make; no installation targets.
SHELL := /bin/bash
.SHELLFLAGS := -eu -o pipefail -c
.DEFAULT_GOAL := all
.DELETE_ON_ERROR:

PYTHON ?= python3
CURL ?= curl
UNZIP ?= unzip
DATA_BUILD_DIR ?= local/data-build
BATHYMETRY_DIR ?= web/bathymetry/channel-2024

EMODNET_URL := https://downloads.emodnet-bathymetry.eu/v12/E4_2024.nc.zip
EMODNET_ARCHIVE := $(DATA_BUILD_DIR)/emodnet-2024/E4_2024.nc.zip
EMODNET_NETCDF := $(DATA_BUILD_DIR)/emodnet-2024/E4_2024.nc
BATHYMETRY_TILES := $(foreach y,96 97 98 99 100 101 102 103,$(foreach x,-13 -12 -11 -10 -9 -8 -7 -6 -5 -4 -3 -2 -1 0 1 2 3 4 5 6,$(BATHYMETRY_DIR)/$(x)_$(y).bin.gz))
BATHYMETRY_OUTPUTS := $(BATHYMETRY_TILES) $(BATHYMETRY_DIR)/manifest.json

.PHONY: all help bathymetry bathymetry-download check check-platform check-download check-bathymetry

# Add future tidal-data targets to this aggregate.
all: bathymetry

help:
	@printf '%s\n' \
	  'make                      Prepare all static datasets (currently bathymetry).' \
	  'make bathymetry           Download/extract EMODnet and generate missing or outdated tiles.' \
	  'make bathymetry-download  Download and extract the EMODnet E4 source only.' \
	  'make check                Check Fedora version and required software; install nothing.' \
	  '' \
	  'Overrides: PYTHON, CURL, UNZIP, DATA_BUILD_DIR, BATHYMETRY_DIR.'

check: check-download check-bathymetry

check-platform:
	@source /etc/os-release; \
	  if [[ "$${ID:-}" != fedora || ! "$${VERSION_ID:-}" =~ ^[0-9]+$$ ]] || (( VERSION_ID < 44 )); then \
	    echo 'Supported build platform: Fedora 44 or later.' >&2; exit 1; \
	  fi

check-download: | check-platform
	@for tool in "$(CURL)" "$(UNZIP)"; do \
	  command -v "$$tool" >/dev/null || { echo "Required command missing: $$tool (nothing was installed)." >&2; exit 1; }; \
	done

check-bathymetry: | check-platform
	@command -v "$(PYTHON)" >/dev/null || { echo 'Python 3 is required (nothing was installed).' >&2; exit 1; }
	@"$(PYTHON)" -c 'import ctypes, ctypes.util, numpy; ctypes.CDLL(ctypes.util.find_library("netcdf") or "libnetcdf.so")' \
	  || { echo 'Bathymetry requires Python NumPy and system libnetcdf (Fedora: python3-numpy, netcdf). Nothing was installed.' >&2; exit 1; }

bathymetry-download: $(EMODNET_NETCDF)

bathymetry: $(BATHYMETRY_OUTPUTS)

# Publish only complete, validated downloads/extractions. Temporary files live
# alongside their destination so rename is atomic, including on another volume.
$(EMODNET_ARCHIVE): | check-download
	@mkdir -p "$(@D)"
	@tmp=$$(mktemp "$@.XXXXXX"); trap 'rm -f "$$tmp"' EXIT; \
	  "$(CURL)" --fail --location --retry 3 --output "$$tmp" "$(EMODNET_URL)"; \
	  "$(UNZIP)" -tq "$$tmp"; \
	  mv "$$tmp" "$@"

$(EMODNET_NETCDF): $(EMODNET_ARCHIVE) | check-download
	@tmp=$$(mktemp "$@.XXXXXX"); trap 'rm -f "$$tmp"' EXIT; \
	  "$(UNZIP)" -p "$<" E4_2024.nc > "$$tmp"; \
	  test -s "$$tmp"; \
	  mv "$$tmp" "$@"

# Grouped targets run the generator once, even with make -j. Listing every
# output (rather than only a stamp) also repairs an individually deleted tile.
$(BATHYMETRY_OUTPUTS) &: $(EMODNET_NETCDF) tools/build-bathymetry.py Makefile | check-bathymetry
	"$(PYTHON)" tools/build-bathymetry.py "$(EMODNET_NETCDF)" --output "$(BATHYMETRY_DIR)"
