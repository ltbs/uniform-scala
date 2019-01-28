
$(document).ready(function() {
    $('[data-target] > input[type="radio"]').change(function () {
	radioValue=this.value;
	dataTarget=$(this).parent("[data-target]").attr("data-target");
	$(".conditional-" + dataTarget).removeClass("govuk-radios__conditional");
	$(".conditional-" + dataTarget).addClass("govuk-radios__conditional--hidden");
	$("#conditional-" + dataTarget + "-" + radioValue).removeClass("govuk-radios__conditional--hidden")
	$("#conditional-" + dataTarget + "-" + radioValue).addClass("govuk-radios__conditional")	
    });
    
});
