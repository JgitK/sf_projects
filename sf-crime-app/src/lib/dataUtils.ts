import { parse } from 'date-fns';
import { VIOLENT_CRIMES, PROPERTY_CRIMES } from './types';
import type { CrimeIncident, FilterState } from './types';

export function processCrimeData(rawData: any[]): CrimeIncident[] {
  return rawData.map((row) => {
    const category = normalizeCategoryName(row['Incident Category']);
    const violent = VIOLENT_CRIMES.includes(category);
    const property = PROPERTY_CRIMES.includes(category);

    const date = new Date(row['Incident Date']);

    return {
      id: row['Incident ID'],
      date,
      year: row['Incident Year'],
      month: date.getMonth() + 1, // 1-12
      neighborhood: row['Analysis Neighborhood'],
      category,
      lat: parseFloat(row['Latitude']),
      long: parseFloat(row['Longitude']),
      violent,
      property,
      color: violent ? 'red' : property ? 'yellow' : '#365188'
    };
  });
}

function normalizeCategoryName(category: string): string {
  const categoryMap: Record<string, string> = {
    "Human Trafficking (A), Commercial Sex Acts": "Human Trafficking (A)",
    "Human Trafficking (B), Involuntary Servitude": "Human Trafficking (B)"
  };

  return categoryMap[category] || category;
}

export function filterIncidents(
  incidents: CrimeIncident[],
  filters: FilterState
): CrimeIncident[] {
  return incidents.filter((incident) => {
    // Neighborhood filter
    if (filters.neighborhoods.length > 0 &&
        !filters.neighborhoods.includes(incident.neighborhood)) {
      return false;
    }

    // Date range filter
    if (incident.date < filters.dateRange[0] ||
        incident.date > filters.dateRange[1]) {
      return false;
    }

    // Crime type filter
    if (filters.crimeType === 'Part 1 Violent Crime' && !incident.violent) {
      return false;
    }
    if (filters.crimeType === 'Part 1 Property Crime' && !incident.property) {
      return false;
    }

    return true;
  });
}

export function getMonthlyCount(incidents: CrimeIncident[]): { month: number; count: number }[] {
  const monthlyCounts: Record<number, number> = {};

  incidents.forEach((incident) => {
    const month = incident.month;
    monthlyCounts[month] = (monthlyCounts[month] || 0) + 1;
  });

  // Return array with all 12 months, even if count is 0
  return Array.from({ length: 12 }, (_, i) => ({
    month: i + 1,
    count: monthlyCounts[i + 1] || 0
  }));
}

export function getUniqueNeighborhoods(incidents: CrimeIncident[]): string[] {
  const neighborhoods = new Set<string>();
  incidents.forEach((incident) => {
    if (incident.neighborhood) {
      neighborhoods.add(incident.neighborhood);
    }
  });
  return Array.from(neighborhoods).sort();
}
