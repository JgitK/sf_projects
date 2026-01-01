export interface CrimeIncident {
  id: string;
  date: Date;
  year: number;
  month: number;
  neighborhood: string;
  category: string;
  lat: number;
  long: number;
  violent: boolean;
  property: boolean;
  color: string;
}

export interface MonthlyCount {
  month: number;
  count: number;
}

export interface FilterState {
  neighborhoods: string[];
  dateRange: [Date, Date];
  crimeType: 'All' | 'Part 1 Violent Crime' | 'Part 1 Property Crime';
}

export const VIOLENT_CRIMES = [
  "Homicide",
  "Rape",
  "Robbery",
  "Assault",
  "Human Trafficking (B)",
  "Human Trafficking (A)"
];

export const PROPERTY_CRIMES = [
  "Burglary",
  "Larceny Theft",
  "Motor Vehicle Theft",
  "Arson"
];
