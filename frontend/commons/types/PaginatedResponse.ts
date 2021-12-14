export interface PaginatedResponse<T> {
  items: Array<T>;
  page: number;
  next_page: number | null;
  total_item_count: number;
  max_items_per_page: number;
}
