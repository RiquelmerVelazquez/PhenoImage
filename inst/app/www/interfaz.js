(function () {
  function setHeaderColor(color) {
    if (!/^#[0-9a-f]{6}$/i.test(color)) return;
    var channels = [1, 3, 5].map(function (i) {
      var value = parseInt(color.slice(i, i + 2), 16) / 255;
      return value <= .04045 ? value / 12.92 : Math.pow((value + .055) / 1.055, 2.4);
    });
    var luminance = .2126 * channels[0] + .7152 * channels[1] + .0722 * channels[2];
    var foreground = luminance > .179 ? '#000000' : '#ffffff';
    var root = document.documentElement;
    root.style.setProperty('--header-background', color);
    root.style.setProperty('--header-foreground', foreground);
    root.style.setProperty('--header-logo-filter', foreground === '#ffffff' ? 'brightness(0) invert(1)' : 'none');
    document.querySelectorAll('[data-header-color]').forEach(function (button) {
      button.setAttribute('aria-pressed', String(button.dataset.headerColor.toLowerCase() === color.toLowerCase()));
    });
    var picker = document.getElementById('color_cabecera');
    if (picker) picker.value = color;
  }
  function resizePanels() {
    window.setTimeout(function () { window.dispatchEvent(new Event('resize')); }, 220);
  }
  function updateHomeLayout() {
    var home = document.querySelector('.home-dashboard');
    document.body.classList.toggle('home-visible', !!home && home.getClientRects().length > 0);
  }
  function setupPanels() {
    document.querySelectorAll('.card').forEach(function (card) {
      if (card.classList.contains('panel-ready')) return;
      var heading = Array.from(card.children).find(function (n) { return n.classList.contains('card-heading'); });
      if (!heading || !heading.querySelector('.panel-collapse')) return;
      var body = document.createElement('div');
      body.className = 'panel-body';
      var sibling = heading.nextSibling;
      while (sibling) {
        var next = sibling.nextSibling;
        body.appendChild(sibling);
        sibling = next;
      }
      card.appendChild(body);
      card.classList.add('panel-ready');
    });
  }
  document.addEventListener('DOMContentLoaded', function () {
    setupPanels();
    updateHomeLayout();
    var pendingLayout = false;
    new MutationObserver(function (records) {
      var relevant = records.some(function (record) {
        var target = record.target.nodeType === 1 ? record.target : record.target.parentElement;
        return !target || !target.closest('.eda-interactive-plot, .selectize-control, .testigo-controls');
      });
      if (!relevant || pendingLayout) return;
      pendingLayout = true;
      window.requestAnimationFrame(function () {
        pendingLayout = false;
        setupPanels();
        updateHomeLayout();
      });
    }).observe(document.querySelector('.workspace') || document.body, {
      childList: true, subtree: true, attributes: true, attributeFilter: ['class']
    });
  });
  document.addEventListener('click', function (event) {
    if (event.target.closest('#mapa_recargar') && window.Shiny && Shiny.setInputValue) {
      Shiny.setInputValue('mapa_error_cliente', '', { priority: 'event' });
    }
    var swatch = event.target.closest('[data-header-color]');
    if (swatch) {
      setHeaderColor(swatch.dataset.headerColor);
      var palette = swatch.closest('details');
      if (palette) { palette.open = false; palette.querySelector('summary').focus(); }
    }
    if (!event.target.closest('.header-palette')) {
      document.querySelectorAll('.header-palette[open]').forEach(function (palette) { palette.open = false; });
    }
    var toggle = event.target.closest('#toggle_menu');
    if (toggle) {
      var collapsed = document.body.classList.toggle('menu-collapsed');
      toggle.setAttribute('aria-expanded', String(!collapsed));
      resizePanels();
    }
    var full = event.target.closest('#pantalla_completa');
    if (full) {
      if (document.fullscreenElement) {
        if (document.exitFullscreen) document.exitFullscreen().catch(function () {});
      } else if (document.documentElement.requestFullscreen) {
        document.documentElement.requestFullscreen().catch(function () {});
      }
    }
    var button = event.target.closest('.panel-collapse');
    if (button) {
      var card = button.closest('.card');
      if (!card) return;
      var closed = card.classList.toggle('panel-closed');
      button.setAttribute('aria-expanded', String(!closed));
      button.setAttribute('aria-label', closed ? 'Expandir panel' : 'Contraer panel');
      button.title = closed ? 'Expandir panel' : 'Contraer panel';
      resizePanels();
    }
  });
  document.addEventListener('toggle', resizePanels, true);
  document.addEventListener('input', function (event) {
    if (event.target.id === 'color_cabecera') setHeaderColor(event.target.value);
  });
  document.addEventListener('keydown', function (event) {
    if (event.key === 'Escape') document.querySelectorAll('.header-palette[open]').forEach(function (palette) {
      palette.open = false; palette.querySelector('summary').focus();
    });
  });
  document.addEventListener('fullscreenchange', resizePanels);
  window.addEventListener('error', function (event) {
    var details = String(event.filename || '') + ' ' + String(event.error && event.error.stack || '');
    if (event.message && /leaflet|mapa_parcelas/i.test(details) && document.getElementById('mapa_parcelas') && window.Shiny && Shiny.setInputValue) {
      Shiny.setInputValue('mapa_error_cliente', String(event.message).slice(0, 400), { priority: 'event' });
    }
  });
  $(document).on('shiny:connected', function () {
    Shiny.addCustomMessageHandler('menuActivo', function (pagina) {
      $('.nav-button').removeClass('active').attr('aria-current', 'false');
      $('#nav_' + pagina).addClass('active').attr('aria-current', 'page');
      window.requestAnimationFrame(updateHomeLayout);
    });
    Shiny.addCustomMessageHandler('procesando', function (ocupado) {
      $('#procesar').prop('disabled', ocupado).attr('aria-busy', ocupado ? 'true' : 'false');
    });
  });
  $(document).on('shiny:value', function (event) {
    if (event.name === 'mapa_parcelas' && event.value && window.Shiny && Shiny.setInputValue) {
      Shiny.setInputValue('mapa_error_cliente', '', { priority: 'event' });
    }
  });
})();

// Sincroniza el color entre los controles de Resultados y Exploración.
(function () {
  function bindTestigoColor() {
    if (!window.Shiny || !Shiny.addCustomMessageHandler) return;
    Shiny.addCustomMessageHandler('cipat-testigo-color', function (data) {
      var input = document.getElementById(data.id);
      if (input && /^#[0-9a-f]{6}$/i.test(data.value)) input.value = data.value;
    });
  }
  if (window.Shiny) bindTestigoColor();
  else if (window.jQuery) jQuery(document).one('shiny:connected', bindTestigoColor);
})();
