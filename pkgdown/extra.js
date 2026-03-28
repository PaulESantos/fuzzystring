(function() {
  function addDevStatus() {
    const sidebar = document.querySelector('.pkgdown-sidebar');
    if (!sidebar) {
      setTimeout(addDevStatus, 100);
      return;
    }

    if (sidebar.querySelector('.dev-status-section')) {
      return;
    }

    const citationHeadings = Array.from(sidebar.querySelectorAll('h3'));
    const citationSection = citationHeadings.find(function(h) {
      return h.textContent.trim() === 'Citation';
    });

    if (!citationSection) {
      return;
    }

    const devStatusDiv = document.createElement('div');
    devStatusDiv.className = 'dev-status-section';
    devStatusDiv.innerHTML =
      '<h3>Dev status</h3>' +
      '<div class="dev-status-badges">' +
      '  <a href="https://lifecycle.r-lib.org/articles/stages.html" title="Lifecycle: stable">' +
      '    <img src="https://img.shields.io/badge/lifecycle-stable-blue.svg" alt="lifecycle: stable">' +
      '  </a>' +
      '  <a href="https://CRAN.R-project.org/package=fuzzystring" title="CRAN status">' +
      '    <img src="https://www.r-pkg.org/badges/version/fuzzystring" alt="CRAN status">' +
      '  </a>' +
      '  <a href="https://cran.r-project.org/package=fuzzystring" title="Total downloads">' +
      '    <img src="http://cranlogs.r-pkg.org/badges/grand-total/fuzzystring?color=blue" alt="downloads">' +
      '  </a>' +
      '  <a href="https://cran.r-project.org/package=fuzzystring" title="Last week downloads">' +
      '    <img src="http://cranlogs.r-pkg.org/badges/last-week/fuzzystring?color=blue" alt="downloads/week">' +
      '  </a>' +
      '  <a href="https://github.com/PaulESantos/fuzzystring/actions/workflows/R-CMD-check.yaml" title="R-CMD-check">' +
      '    <img src="https://github.com/PaulESantos/fuzzystring/actions/workflows/R-CMD-check.yaml/badge.svg" alt="R-CMD-check">' +
      '  </a>' +
      '</div>';

    const citationParent = citationSection.parentElement;
    if (citationParent && citationParent.nextElementSibling) {
      citationParent.nextElementSibling.insertAdjacentElement('afterend', devStatusDiv);
    } else if (citationParent) {
      citationParent.insertAdjacentElement('afterend', devStatusDiv);
    }
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', addDevStatus);
  } else {
    addDevStatus();
  }

  setTimeout(addDevStatus, 500);
})();
