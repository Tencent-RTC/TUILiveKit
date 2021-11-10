<template lang="pug">
  div.video-record {{ $t('Coming soon') }}
</template>

<script>
export default {
  name: 'video-record',
  data() {
    return {

    };
  },
};
</script>

<style lang="stylus" scoped>
  .video-record
    width 100%
    height 100%
    color $grayFontColor
    padding-top 120px
    font-size 16px
</style>

<i18n>
{
	"en": {
		"Coming soon": "Coming soon"
	},
	"zh": {
		"Coming soon": "敬请期待"
	}
}
</i18n>
