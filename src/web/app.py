from fastapi import FastAPI, Request
from fastapi.templating import Jinja2Templates
from fastapi.staticfiles import StaticFiles
from pathlib import Path

from probability.warhammer.profile import DEFAULT_PROFILE, DEFAULT_DEFENDER
from probability.warhammer.attack import roll_to_hit

app = FastAPI()

# Setup templates and static files
BASE_DIR = Path(__file__).resolve().parent.parent.parent
templates = Jinja2Templates(directory=str(BASE_DIR / "templates"))
app.mount("/static", StaticFiles(directory=str(BASE_DIR / "static")), name="static")

@app.get("/")
async def home(request: Request):
    # Example probability calculation
    hit_dist = roll_to_hit(DEFAULT_PROFILE, DEFAULT_DEFENDER, [])
    return templates.TemplateResponse(
        "index.html",
        {
            "request": request,
            "hit_distribution": str(hit_dist),
        }
    ) 