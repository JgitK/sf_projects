<script lang="ts">
  import { format } from 'date-fns';

  export let startDate: Date;
  export let endDate: Date;
  export let label: string = 'Date Range';

  let startDateStr = format(startDate, 'yyyy-MM-dd');
  let endDateStr = format(endDate, 'yyyy-MM-dd');

  $: if (startDateStr) {
    startDate = new Date(startDateStr);
  }

  $: if (endDateStr) {
    endDate = new Date(endDateStr);
  }
</script>

<div class="flex flex-col gap-2">
  <label class="text-sm font-medium text-white">
    {label}
  </label>
  <div class="flex gap-2">
    <input
      type="date"
      bind:value={startDateStr}
      class="px-3 py-2 bg-gray-700 text-white rounded-md border border-gray-600 focus:outline-none focus:ring-2 focus:ring-blue-500"
    />
    <span class="self-center text-white">to</span>
    <input
      type="date"
      bind:value={endDateStr}
      class="px-3 py-2 bg-gray-700 text-white rounded-md border border-gray-600 focus:outline-none focus:ring-2 focus:ring-blue-500"
    />
  </div>
</div>
