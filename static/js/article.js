// Script loaded by posts for interactive formatting elements

function makeAsideInteractive(aside) {
    aside.classList.add('aside-hidden');
    aside.addEventListener("click", () => {
        aside.classList.toggle('aside-hidden');
    });
}

function makeAsidesInteractive() {
    const asides = document.querySelectorAll('aside');
    for (const aside of asides) {
        console.log("aside is becoming interactive!");
        makeAsideInteractive(aside);
    }
}

addEventListener("DOMContentLoaded", () => {
    makeAsidesInteractive();
});
