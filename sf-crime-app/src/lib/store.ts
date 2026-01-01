import { writable, derived } from 'svelte/store';
import type { CrimeIncident, FilterState } from './types';
import { filterIncidents } from './dataUtils';

// Crime data store
export const crimeData = writable<CrimeIncident[]>([]);

// Filter state store
export const filters = writable<FilterState>({
  neighborhoods: [],
  dateRange: [new Date('2023-01-01'), new Date('2023-12-10')],
  crimeType: 'All'
});

// Derived store for filtered data
export const filteredData = derived(
  [crimeData, filters],
  ([$crimeData, $filters]) => {
    return filterIncidents($crimeData, $filters);
  }
);

// Loading state
export const isLoading = writable<boolean>(true);
