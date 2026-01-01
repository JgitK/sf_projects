<script lang="ts">
  import { onMount, onDestroy } from 'svelte';
  import mapboxgl from 'mapbox-gl';
  import 'mapbox-gl/dist/mapbox-gl.css';
  import type { CrimeIncident } from '../lib/types';

  export let data: CrimeIncident[] = [];
  export let accessToken: string = '';

  let mapContainer: HTMLDivElement;
  let map: mapboxgl.Map | null = null;

  $: if (map && data) {
    updateMapData();
  }

  function initializeMap() {
    if (!accessToken) {
      console.warn('Mapbox access token not provided');
      return;
    }

    mapboxgl.accessToken = accessToken;

    map = new mapboxgl.Map({
      container: mapContainer,
      style: 'mapbox://styles/mapbox/dark-v11',
      center: [-122.4254, 37.7743],
      zoom: 11
    });

    map.on('load', () => {
      updateMapData();
    });

    // Add navigation controls
    map.addControl(new mapboxgl.NavigationControl(), 'top-left');

    // Add scale control
    map.addControl(
      new mapboxgl.ScaleControl({
        maxWidth: 80,
        unit: 'imperial'
      }),
      'bottom-left'
    );
  }

  function updateMapData() {
    if (!map || !map.isStyleLoaded()) return;

    const geojson: GeoJSON.FeatureCollection = {
      type: 'FeatureCollection',
      features: data.map((incident) => ({
        type: 'Feature',
        geometry: {
          type: 'Point',
          coordinates: [incident.long, incident.lat]
        },
        properties: {
          id: incident.id,
          category: incident.category,
          date: incident.date.toLocaleDateString(),
          color: incident.color
        }
      }))
    };

    // Remove existing source and layers
    if (map.getLayer('incidents-layer')) {
      map.removeLayer('incidents-layer');
    }
    if (map.getLayer('clusters')) {
      map.removeLayer('clusters');
    }
    if (map.getLayer('cluster-count')) {
      map.removeLayer('cluster-count');
    }
    if (map.getSource('incidents')) {
      map.removeSource('incidents');
    }

    // Add source with clustering
    map.addSource('incidents', {
      type: 'geojson',
      data: geojson,
      cluster: true,
      clusterMaxZoom: 12,
      clusterRadius: 50
    });

    // Add cluster circles layer
    map.addLayer({
      id: 'clusters',
      type: 'circle',
      source: 'incidents',
      filter: ['has', 'point_count'],
      paint: {
        'circle-color': '#AD7D2E',
        'circle-radius': 18,
        'circle-blur': 0.3,
        'circle-stroke-color': 'white',
        'circle-stroke-width': 1,
        'circle-stroke-opacity': 1
      }
    });

    // Add cluster count labels
    map.addLayer({
      id: 'cluster-count',
      type: 'symbol',
      source: 'incidents',
      filter: ['has', 'point_count'],
      layout: {
        'text-field': '{point_count_abbreviated}',
        'text-size': 12
      }
    });

    // Add unclustered points layer
    map.addLayer({
      id: 'incidents-layer',
      type: 'circle',
      source: 'incidents',
      filter: ['!', ['has', 'point_count']],
      paint: {
        'circle-radius': 3,
        'circle-color': ['get', 'color']
      }
    });

    // Add popup on click
    map.on('click', 'incidents-layer', (e) => {
      if (!e.features || e.features.length === 0) return;

      const coordinates = (e.features[0].geometry as any).coordinates.slice();
      const properties = e.features[0].properties;

      // Ensure that if the map is zoomed out such that multiple
      // copies of the feature are visible, the popup appears
      // over the copy being pointed to.
      while (Math.abs(e.lngLat.lng - coordinates[0]) > 180) {
        coordinates[0] += e.lngLat.lng > coordinates[0] ? 360 : -360;
      }

      new mapboxgl.Popup()
        .setLngLat(coordinates)
        .setHTML(
          `<div class="text-sm">
            <strong>Incident Category:</strong> ${properties?.category}<br/>
            <strong>Date:</strong> ${properties?.date}
          </div>`
        )
        .addTo(map!);
    });

    // Change cursor on hover
    map.on('mouseenter', 'incidents-layer', () => {
      if (map) map.getCanvas().style.cursor = 'pointer';
    });

    map.on('mouseleave', 'incidents-layer', () => {
      if (map) map.getCanvas().style.cursor = '';
    });
  }

  onMount(() => {
    initializeMap();
  });

  onDestroy(() => {
    if (map) {
      map.remove();
    }
  });
</script>

<div class="w-full h-full rounded overflow-hidden" bind:this={mapContainer}></div>
