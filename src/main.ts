/* Static Assets */
import "maplibre-gl/dist/maplibre-gl.css";
import "./style.css";

/* Imports */
import { LogoControl } from "./controls/LogoControl";
import { Protocol } from "pmtiles";
import maplibregl, { NavigationControl } from "maplibre-gl";


// ----------------------------------------------------------------------------
// Config
// ----------------------------------------------------------------------------

// URL of PMTiles store.
const tiles_url = "/reference.reservoirs/reference.pmtiles";

// Layer in the PMTiles store to visualize.
const tiles_layers = [
    "reference-reservoirs"
] as const;

// Some types to help with auto complete
type Layer = typeof tiles_layers[number];
type PaintSpecification = Pick<maplibregl.AddLayerObject, "type"> & Pick<maplibregl.LayerSpecification, "paint">
type TilePaintSpecification = Record<Layer, PaintSpecification>;

// Paint specification for the tiles layer
const tiles_paint: TilePaintSpecification = {
    "reference-reservoirs": {
        type: "circle",
    paint: {
      // Circle radius scales smoothly with zoom
      "circle-radius": [
        "interpolate",
        ["linear"],
        ["zoom"],
        4, 2.5,
        8, 4,
        12, 7
      ],

      // Strong but neutral dam color
      "circle-color": "#8B4513", // saddle brown

      // White outline for visibility against any background
      "circle-stroke-color": "white",
      "circle-stroke-width": 1.2,

      // Slight transparency so clusters of dams don't overpower the map
      "circle-opacity": 0.85
    }
  }
}

/**
 * Format text for a popup given a feature.
 * @param _root The root DOM element (typically a <span> in this case).
 * @param feature The current feature under the mouse at this popup.
 * @returns The inner popup HTML as a string.
 */
async function popupText(
  _root: HTMLElement,
  feature: maplibregl.MapGeoJSONFeature
): Promise<string> {
  const props = feature.properties || {};

  const rows = Object.entries(props)
    .map(
      ([key, value]) => `
        <tr>
          <th style="text-align:left; padding:4px; border:1px solid #ddd; background:#f9f9f9;">
            ${key}
          </th>
          <td style="padding:4px; border:1px solid #ddd;">
            ${String(value)}
          </td>
        </tr>`
    )
    .join("");

  return `
    <div style="max-height:200px; overflow:auto;">
      <table style="border-collapse:collapse; font-family:sans-serif; font-size:13px; width:100%;">
        <tbody>
          ${rows}
        </tbody>
      </table>
    </div>`;
}

// ----------------------------------------------------------------------------

// Add PMTiles protocol to MapLibre
const pmtiles = new Protocol();
maplibregl.addProtocol("pmtiles", pmtiles.tile);

const map = new maplibregl.Map({
    container: "map",
    style: "https://basemaps.cartocdn.com/gl/positron-gl-style/style.json",
    center: [-98.583333, 39.833333] as maplibregl.LngLatLike,
    zoom: 4
});

// Initialize Popup Requirements
type MouseEvent = maplibregl.MapMouseEvent & {
    features?: maplibregl.MapGeoJSONFeature[];
}

const popupRoot = document.createElement("span");

async function popupEvent(event: MouseEvent) {
    if (event.features === undefined || event.features.length === 0) return;

    // The first feature under the mouse
    const currentFeature = event.features[0];

    popupRoot.innerHTML = await popupText(popupRoot, currentFeature);

    new maplibregl.Popup()
        .setMaxWidth("none")
        .setLngLat(event.lngLat)
        .setDOMContent(popupRoot)
        .addTo(map);
        
    console.log(currentFeature);
}

let hoverFeatureId: string | number | undefined;
function hoverSet(id: typeof hoverFeatureId, value: boolean): void {
    map.setFeatureState(
        { sourceLayer: "reference-reservoirs", source: "default", id: id },
        { hover: value }
    );
}

async function mouseEnterEvent(_event: MouseEvent): Promise<void> {
    map.getCanvas().style.cursor = "pointer";
}

async function mouseMoveEvent(event: MouseEvent): Promise<void> {
    if (event.features === undefined || event.features.length === 0) return;

    if (hoverFeatureId) {
        hoverSet(hoverFeatureId, false);
    }

    hoverFeatureId = event.features[0].id;
    hoverSet(hoverFeatureId, true);
}

async function mouseLeaveEvent(_event: MouseEvent): Promise<void> {
    map.getCanvas().style.cursor = "";

    if (hoverFeatureId) {
        hoverSet(hoverFeatureId, false);
    }

    hoverFeatureId = undefined;
}

/* Map Events */
map.once("load", async () => {
    map.addControl(new NavigationControl(), "top-right");
    map.addControl(new LogoControl(), "bottom-left");
    map.addSource("default", {
        type: "vector",
        url: `pmtiles://${tiles_url}`,
        attribution: '&copy; <a href="https://www.lynker-spatial.com" target="_blank" rel="noopener">Lynker Spatial</a>'
    });

    for (const layer of tiles_layers) {
        // @ts-expect-error: tiles_paint will work, but TS does not like it.
        map.addLayer({
            id: "default",
            source: "default",
            "source-layer": layer,
            type: tiles_paint[layer].type,
            paint: tiles_paint[layer].paint
        });
        
        console.log(`Added layer: ${layer}`);
    }

    map.on("click", "default", popupEvent);
    map.on("mouseenter", "default", mouseEnterEvent)
    map.on("mousemove", "default", mouseMoveEvent);
    map.on("mouseleave", "default", mouseLeaveEvent);
    
    map.addSource("flowpaths", {
      type: "vector",
      url: "pmtiles://https://data.lynker-spatial.com/vector-resources/vis/conus-reference.pmtiles",
      attribution: "&copy; <a href='https://www.lynker-spatial.com' target='_blank' rel='noopener'>Lynker Spatial</a>"
    });

    map.addLayer({
        id: "flowpaths",
        type: "line",
        source: "flowpaths",
        "source-layer": "flowpaths",
        layout: {
            'line-cap': 'round',
            'line-join': 'round'
        },
        minzoom: 5,
        paint: {
            'line-opacity': [
                'interpolate', ['linear'], ['zoom'],
                0, 0,
                5, 0,
                6, 1
            ],
            'line-color': '#1565c0',
            'line-width': [
                'interpolate', ['linear'], ['get', 'TotDASqKM'],
                0,      1,
                10,     1.5,
                100,    1.75,
                1000,   2,
                10000,  4,
                100000, 6,
                500000, 8,
                800000, 10
            ]
        }
    });
    
     map.addLayer({
        id: "flowpaths-labels",
        type: "line",
        source: "flowpaths",
        "source-layer": "flowpaths",
        layout: {
            'line-cap': 'round',
            'line-join': 'round'
        },
        minzoom: 5,
        paint: {
            'line-opacity': 0.001,
            'line-width': 10
        }
    });
    

    map.on("click", "flowpaths-labels", popupEvent);

})
