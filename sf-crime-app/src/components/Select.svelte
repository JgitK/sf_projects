<script lang="ts">
  export let value: string = '';
  export let options: string[] = [];
  export let label: string = '';
  export let id: string = '';
</script>

<div class="flex flex-col gap-2">
  {#if label}
    <label for={id} class="text-sm font-medium text-white">
      {label}
    </label>
  {/if}
  <select
    {id}
    bind:value
    class="px-3 py-2 bg-gray-700 text-white rounded-md border border-gray-600 focus:outline-none focus:ring-2 focus:ring-blue-500"
  >
    {#each options as option}
      <option value={option}>{option}</option>
    {/each}
  </select>
</div>
