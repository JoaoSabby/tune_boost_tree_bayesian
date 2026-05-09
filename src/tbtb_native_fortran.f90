subroutine tbtb_pr_auc_f(n, actual, predicted, score)

  implicit none
  integer, intent(in) :: n
  integer, intent(in) :: actual(n)
  double precision, intent(in) :: predicted(n)
  double precision, intent(out) :: score
  integer, allocatable :: order_index(:)
  integer :: i, positives
  double precision :: tp, fp, recall, precision, last_recall

  score = 0.0d0
  if (n < 1) then
    score = -1.0d308
    return
  end if

  positives = 0
  allocate(order_index(n))
  do i = 1, n
    if (actual(i) == 1) positives = positives + 1
    order_index(i) = i
  end do

  if (positives == 0) then
    score = -1.0d308
    deallocate(order_index)
    return
  end if

  call quicksort_indices(order_index, 1, n)

  tp = 0.0d0
  fp = 0.0d0
  last_recall = 0.0d0
  do i = 1, n
    if (actual(order_index(i)) == 1) then
      tp = tp + 1.0d0
    else
      fp = fp + 1.0d0
    end if
    recall = tp / dble(positives)
    precision = tp / (tp + fp)
    score = score + (recall - last_recall) * precision
    last_recall = recall
  end do

  deallocate(order_index)

contains

  logical function before(left_index, right_index)

    integer, intent(in) :: left_index, right_index
    if (predicted(left_index) > predicted(right_index)) then
      before = .true.
    else if (predicted(left_index) < predicted(right_index)) then
      before = .false.
    else
      before = left_index < right_index
    end if
  end function before

  recursive subroutine quicksort_indices(index_vector, left, right)

    integer, intent(inout) :: index_vector(:)
    integer, intent(in) :: left, right
    integer :: i_left, i_right, pivot, temp

    if (left >= right) return
    i_left = left
    i_right = right
    pivot = index_vector((left + right) / 2)

    do
      do while (before(index_vector(i_left), pivot))
        i_left = i_left + 1
      end do
      do while (before(pivot, index_vector(i_right)))
        i_right = i_right - 1
      end do
      if (i_left <= i_right) then
        temp = index_vector(i_left)
        index_vector(i_left) = index_vector(i_right)
        index_vector(i_right) = temp
        i_left = i_left + 1
        i_right = i_right - 1
      end if
      if (i_left > i_right) exit
    end do

    if (left < i_right) call quicksort_indices(index_vector, left, i_right)
    if (i_left < right) call quicksort_indices(index_vector, i_left, right)
  end subroutine quicksort_indices

end subroutine tbtb_pr_auc_f
