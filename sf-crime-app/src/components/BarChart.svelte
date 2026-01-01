<script lang="ts">
  import { onMount } from 'svelte';
  import * as d3 from 'd3';
  import type { CrimeIncident } from '../lib/types';
  import { getMonthlyCount } from '../lib/dataUtils';

  export let data: CrimeIncident[] = [];

  let chartContainer: HTMLDivElement;
  const monthNames = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'];

  $: if (chartContainer && data) {
    renderChart();
  }

  function renderChart() {
    // Clear previous chart
    d3.select(chartContainer).selectAll('*').remove();

    const monthlyCounts = getMonthlyCount(data);

    const margin = { top: 10, right: 10, bottom: 40, left: 40 };
    const width = 600 - margin.left - margin.right;
    const height = 200 - margin.top - margin.bottom;

    const svg = d3
      .select(chartContainer)
      .append('svg')
      .attr('width', width + margin.left + margin.right)
      .attr('height', height + margin.top + margin.bottom)
      .append('g')
      .attr('transform', `translate(${margin.left},${margin.top})`);

    // X scale
    const x = d3
      .scaleBand()
      .domain(monthlyCounts.map((d) => d.month.toString()))
      .range([0, width])
      .padding(0.1);

    // Y scale
    const y = d3
      .scaleLinear()
      .domain([0, d3.max(monthlyCounts, (d) => d.count) || 0])
      .nice()
      .range([height, 0]);

    // Add bars
    svg
      .selectAll('.bar')
      .data(monthlyCounts)
      .enter()
      .append('rect')
      .attr('class', 'bar')
      .attr('x', (d) => x(d.month.toString()) || 0)
      .attr('y', (d) => y(d.count))
      .attr('width', x.bandwidth())
      .attr('height', (d) => height - y(d.count))
      .attr('fill', '#AD7D2E');

    // X axis
    svg
      .append('g')
      .attr('transform', `translate(0,${height})`)
      .call(
        d3
          .axisBottom(x)
          .tickFormat((d) => monthNames[parseInt(d) - 1])
      )
      .selectAll('text')
      .style('text-anchor', 'end')
      .attr('dx', '-.8em')
      .attr('dy', '.15em')
      .attr('transform', 'rotate(-40)')
      .style('fill', 'white');

    // Y axis
    svg
      .append('g')
      .call(d3.axisLeft(y))
      .selectAll('text')
      .style('fill', 'white');

    // Style axes
    svg.selectAll('.domain').style('stroke', 'white');
    svg.selectAll('.tick line').style('stroke', 'white');
  }

  onMount(() => {
    renderChart();
  });
</script>

<div class="w-full bg-[#7A7A7A] p-4 rounded">
  <div bind:this={chartContainer} class="w-full"></div>
</div>
