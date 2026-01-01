<script lang="ts">
  import { onMount } from 'svelte';
  import * as d3 from 'd3';
  import { crimeData, filters, filteredData, isLoading } from './lib/store';
  import { processCrimeData, getUniqueNeighborhoods } from './lib/dataUtils';
  import CrimeMap from './components/CrimeMap.svelte';
  import BarChart from './components/BarChart.svelte';
  import Select from './components/Select.svelte';
  import DateRangePicker from './components/DateRangePicker.svelte';
  import Card from './components/Card.svelte';

  // You'll need to add your Mapbox token here
  const MAPBOX_TOKEN = import.meta.env.VITE_MAPBOX_TOKEN || '';

  let neighborhoods: string[] = [];
  let selectedNeighborhood = 'All';
  let selectedCrimeType = 'All';
  let startDate = new Date('2023-01-01');
  let endDate = new Date('2023-12-10');

  // Reactive statements to update filters when form values change
  $: {
    filters.set({
      neighborhoods: selectedNeighborhood === 'All' ? [] : [selectedNeighborhood],
      dateRange: [startDate, endDate],
      crimeType: selectedCrimeType as any
    });
  }

  onMount(async () => {
    try {
      // Load CSV data - you'll need to copy your CSV file to the public folder
      const response = await fetch('/sf_incidents_new.csv');
      const csvText = await response.text();

      // Parse CSV
      const rawData = d3.csvParse(csvText);

      // Process data
      const processed = processCrimeData(rawData as any[]);

      // Filter for 2023 only (matching original R app)
      const filtered2023 = processed.filter((d) => d.year === 2023);

      crimeData.set(filtered2023);

      // Get unique neighborhoods
      neighborhoods = ['All', ...getUniqueNeighborhoods(filtered2023)];

      isLoading.set(false);
    } catch (error) {
      console.error('Error loading crime data:', error);
      isLoading.set(false);
    }
  });
</script>

<main class="min-h-screen bg-gray-900 p-4">
  <div class="max-w-7xl mx-auto">
    <h1 class="text-3xl font-bold text-white mb-6">
      SF Crime Incident Reports - 2023
    </h1>

    {#if $isLoading}
      <div class="flex items-center justify-center h-96">
        <p class="text-white text-xl">Loading crime data...</p>
      </div>
    {:else}
      <div class="grid grid-cols-1 lg:grid-cols-4 gap-4">
        <!-- Controls Panel -->
        <Card className="p-6 lg:col-span-1 h-fit">
          <h2 class="text-xl font-semibold text-white mb-4">Filters</h2>

          <div class="space-y-4">
            <Select
              bind:value={selectedNeighborhood}
              options={neighborhoods}
              label="Select Neighborhood"
              id="neighborhood"
            />

            <DateRangePicker
              bind:startDate
              bind:endDate
              label="Select Date Range"
            />

            <Select
              bind:value={selectedCrimeType}
              options={['All', 'Part 1 Violent Crime', 'Part 1 Property Crime']}
              label="Crime Type"
              id="crimeType"
            />

            <!-- Legend -->
            <div class="mt-6 pt-6 border-t border-gray-600">
              <h3 class="text-sm font-semibold text-white mb-3">Legend</h3>
              <div class="space-y-2">
                <div class="flex items-center gap-2">
                  <div class="w-4 h-4 rounded-full bg-red-500"></div>
                  <span class="text-sm text-white">Part 1 Violent Crime</span>
                </div>
                <div class="flex items-center gap-2">
                  <div class="w-4 h-4 rounded-full bg-yellow-400"></div>
                  <span class="text-sm text-white">Part 1 Property Crime</span>
                </div>
                <div class="flex items-center gap-2">
                  <div class="w-4 h-4 rounded-full" style="background-color: #365188"></div>
                  <span class="text-sm text-white">Other</span>
                </div>
              </div>
            </div>

            <!-- Stats -->
            <div class="mt-6 pt-6 border-t border-gray-600">
              <h3 class="text-sm font-semibold text-white mb-2">Statistics</h3>
              <p class="text-2xl font-bold text-white">
                {$filteredData.length.toLocaleString()}
              </p>
              <p class="text-sm text-gray-400">Total Incidents</p>
            </div>
          </div>
        </Card>

        <!-- Map and Chart Panel -->
        <div class="lg:col-span-3 space-y-4">
          <!-- Map -->
          <Card className="p-0 overflow-hidden" style="height: 600px;">
            <CrimeMap data={$filteredData} accessToken={MAPBOX_TOKEN} />
          </Card>

          <!-- Bar Chart -->
          <Card className="p-6">
            <h2 class="text-xl font-semibold text-white mb-4">
              Monthly Incident Count
            </h2>
            <BarChart data={$filteredData} />
          </Card>

          <!-- Data Source -->
          <div class="text-center">
            <p class="text-sm text-gray-400">
              Data sourced from <a
                href="https://data.sfgov.org"
                class="text-blue-400 hover:text-blue-300 underline"
                target="_blank"
                rel="noopener noreferrer"
              >
                data.sfgov.org
              </a>
            </p>
          </div>
        </div>
      </div>
    {/if}
  </div>
</main>
