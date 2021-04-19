<script>
$('#copybtn{{ $id }}').click(function(){
  var $temp = $("<input>");
  var brRegex = /<br\s*[\/]?>/gi;
  $("body").append($temp);
  $temp.val($('#{{ $id }}').text()).select();
  // $temp.val($('#{{ $id }}').html().replace(brRegex, "\r\n")).select();
  document.execCommand("copy");
  $temp.remove();
  var $this = $(this);
  $this.text('Copied!');
});
</script>
