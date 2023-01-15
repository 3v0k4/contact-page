let i = 0
const intervalId = setInterval(() => {
  i = (i+1) % document.querySelectorAll('.featureButton').length
  document.querySelector(`#featureButton${i+1}`).dispatchEvent(new Event('programmaticClick'))
}, 5000)

document.querySelectorAll('.featureButton').forEach((element) => {
  element.addEventListener('click', (event) => {
    clearInterval(intervalId)
    document.querySelectorAll('.featureImage').forEach((element) => element.classList.add('hidden'))
    document.querySelector(event.currentTarget.dataset.imgId).classList.remove('hidden')
  })

  element.addEventListener('programmaticClick', (event) => {
    const featureButtons = document.querySelectorAll('.featureButton').forEach((element) => element.parentElement.querySelector('input').checked = false)
    event.currentTarget.parentElement.querySelector('input').checked = true
    document.querySelectorAll('.featureImage').forEach((element) => element.classList.add('hidden'))
    document.querySelector(event.currentTarget.dataset.imgId).classList.remove('hidden')
  })
})

const whenVisible = (elements, callback) => {
  elements.forEach(element => {
    new IntersectionObserver((entries, observer) => {
      entries.forEach(entry => {
        if (!entry.isIntersecting) { return }
        callback(element)
        observer.disconnect()
      }, { root: null, threshold: 0.3 })
    }).observe(element)
  })
}

whenVisible(document.querySelectorAll('.slideIn'), (element) => element.classList.add('slide-in'))
