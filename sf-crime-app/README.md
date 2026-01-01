# SF Crime Visualization App

A modern web application for visualizing San Francisco crime incident data, built with Svelte, D3.js, and Mapbox GL. This is a complete rewrite of the original R Shiny application with improved performance and a modern tech stack.

## Features

- **Interactive Map**: Mapbox GL-powered map with clustering for thousands of crime incidents
- **Dynamic Filtering**: Filter by neighborhood, date range, and crime type
- **Monthly Statistics**: D3.js bar chart showing incident counts by month
- **Real-time Updates**: Reactive UI that updates instantly based on filter selections
- **Responsive Design**: Works on desktop, tablet, and mobile devices
- **Dark Theme**: Modern dark UI with Tailwind CSS

## Tech Stack

- **Svelte 5** - Reactive UI framework
- **TypeScript** - Type-safe development
- **Vite** - Fast build tool and dev server
- **D3.js** - Data visualization library
- **Mapbox GL JS** - Interactive maps
- **Tailwind CSS** - Utility-first CSS framework
- **date-fns** - Date manipulation

## Prerequisites

- Node.js 18+ and npm
- Mapbox account (free tier available)

## Setup

### 1. Install Dependencies

```bash
npm install
```

### 2. Configure Mapbox Token

1. Create a Mapbox account at https://account.mapbox.com/
2. Get your access token from https://account.mapbox.com/access-tokens/
3. Create a `.env` file in the root directory:

```bash
cp .env.example .env
```

4. Edit `.env` and add your token:

```
VITE_MAPBOX_TOKEN=pk.your_actual_token_here
```

### 3. Add Data File

Copy your crime data CSV file to the `public` folder:

```bash
cp ../sf_incidents_new.csv public/
```

The CSV should have the following columns:
- `Incident ID`
- `Incident Date`
- `Incident Year`
- `Analysis Neighborhood`
- `Incident Category`
- `Latitude`
- `Longitude`

### 4. Run Development Server

```bash
npm run dev
```

The app will be available at `http://localhost:5173`

## Building for Production

```bash
npm run build
```

This creates optimized production files in the `dist/` folder.

To preview the production build:

```bash
npm run preview
```

## Project Structure

```
sf-crime-app/
├── src/
│   ├── components/
│   │   ├── BarChart.svelte      # D3.js monthly bar chart
│   │   ├── CrimeMap.svelte      # Mapbox GL map component
│   │   ├── Card.svelte          # UI card component
│   │   ├── Select.svelte        # Select dropdown component
│   │   └── DateRangePicker.svelte # Date range input
│   ├── lib/
│   │   ├── types.ts             # TypeScript type definitions
│   │   ├── dataUtils.ts         # Data processing utilities
│   │   └── store.ts             # Svelte stores for state management
│   ├── App.svelte               # Main application component
│   ├── app.css                  # Tailwind CSS imports
│   └── main.ts                  # Application entry point
├── public/
│   └── sf_incidents_new.csv     # Crime data (add your own)
├── package.json
├── vite.config.ts
├── tailwind.config.js
└── tsconfig.json
```

## Performance Features

This app includes several performance optimizations:

- **Efficient Data Processing**: Single-pass data transformations
- **Optimized Filtering**: Sequential filter evaluation with early exits
- **Reactive State Management**: Svelte's built-in reactivity for minimal re-renders
- **Map Clustering**: Automatic clustering of nearby points for better performance
- **Lazy Loading**: Components load only when needed

## Key Components

### CrimeMap
- Displays crime incidents on an interactive Mapbox GL map
- Features clustering for better performance with large datasets
- Color-coded points: red (violent crimes), yellow (property crimes), blue (other)
- Click on points to see incident details in popup

### BarChart
- D3.js visualization of monthly incident counts
- Automatically updates based on filtered data
- Responsive design that works on all screen sizes

### Filters
- **Neighborhood**: Select specific neighborhood or "All"
- **Date Range**: Pick start and end dates for filtering
- **Crime Type**: Filter by violent crimes, property crimes, or all

## Data Processing

The app processes crime data with the following categorizations:

**Violent Crimes (Part 1):**
- Homicide
- Rape
- Robbery
- Assault
- Human Trafficking (A)
- Human Trafficking (B)

**Property Crimes (Part 1):**
- Burglary
- Larceny Theft
- Motor Vehicle Theft
- Arson

## Customization

### Updating Crime Categories

Edit `src/lib/types.ts` to modify crime categorizations:

```typescript
export const VIOLENT_CRIMES = [
  "Homicide",
  // Add or remove categories
];
```

### Changing Map Style

Edit `src/components/CrimeMap.svelte` to change the Mapbox style:

```typescript
style: 'mapbox://styles/mapbox/streets-v12', // or other Mapbox styles
```

### Adjusting Colors

Edit `src/lib/dataUtils.ts` to change incident colors:

```typescript
color: violent ? 'red' : property ? 'yellow' : '#365188'
```

## Environment Variables

- `VITE_MAPBOX_TOKEN`: Your Mapbox access token (required)

## Browser Support

- Chrome 90+
- Firefox 88+
- Safari 14+
- Edge 90+

## Troubleshooting

### Map not loading
- Verify your Mapbox token is correctly set in `.env`
- Check browser console for errors
- Ensure you're not exceeding Mapbox's free tier limits

### Data not loading
- Verify CSV file is in `public/sf_incidents_new.csv`
- Check that CSV columns match expected format
- Open browser console to see error messages

### Build errors
- Delete `node_modules` and `package-lock.json`
- Run `npm install` again
- Ensure you're using Node.js 18+

## Credits

- Original R Shiny app: Performance analysis and optimization insights
- Data source: [SF OpenData](https://data.sfgov.org)
- Mapbox GL JS for mapping
- D3.js for visualizations
- Svelte for the reactive UI framework

---

Built with ❤️ using modern web technologies
