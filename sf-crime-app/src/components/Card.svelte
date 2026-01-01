<script lang="ts">
  export let className: string = '';
</script>

<div class="bg-gray-100 dark:bg-gray-800 rounded-lg border border-gray-200 dark:border-gray-700 {className}">
  <slot />
</div>
