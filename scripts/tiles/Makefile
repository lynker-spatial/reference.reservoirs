INPUT = ../../output/reference-reservoirs.gpkg
OUTPUT = ../../public/reference.pmtiles
LAYERS = reference-reservoirs

TILEJOIN=tile-join
TIPPECANOE=tippecanoe
TIPPECANOE_OPTS=-zg --coalesce-densest-as-needed --simplify-only-low-zooms --no-simplification-of-shared-nodes --extend-zooms-if-still-dropping --visvalingam --hilbert

.DEFAULT_GOAL := $(OUTPUT)

$(OUTPUT): $(LAYERS:=.mbtiles)
	$(TILEJOIN) -pk -o $@ $^

%.fgb: $(INPUT)
	ogr2ogr -f FlatGeobuf -t_srs "EPSG:4326" $@ $< $(basename $@)

%.mbtiles: %.fgb
	$(TIPPECANOE) -f -o $@ $(TIPPECANOE_OPTS) -L $(basename $@):$< || true

clean:
	fm -f *.fgb *.mbtiles $(OUTPUT)
	
.PHONY: clean
