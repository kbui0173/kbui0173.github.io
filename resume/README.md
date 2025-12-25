# Resume/CV Folder

This folder contains the Expressive Resume LaTeX template for creating professional resumes.

## Files

- `expressive-resume/` - Professional Expressive Resume template
  - `src/my_resume.tex` - Your customized resume (edit this file)
  - `src/my_resume.pdf` - Your compiled resume PDF
  - `src/ExpressiveResume.cls` - Template class file
  - `src/images/qr.png` - QR code placeholder
- `README.md` - This documentation

## Quick Start

1. **Edit your resume:**
   ```bash
   # Open the template
   open expressive-resume/src/my_resume.tex
   ```

2. **Replace placeholder information** with your actual details:
   - Name, contact info, education
   - Experience, skills, projects

3. **Compile to PDF:**
   ```bash
   cd expressive-resume/src
   pdflatex my_resume.tex
   ```

4. **Your resume is ready!** Check `my_resume.pdf`

## Expressive Resume Features

- ✅ **Professional Design** - Clean, modern layout with icons
- ✅ **ATS-Friendly** - Optimized for Applicant Tracking Systems
- ✅ **Easy to Use** - Simple semantic commands instead of complex LaTeX
- ✅ **No LaTeX Knowledge Required** - Just edit the content!

## Template Commands

```latex
% Header with contact info
\resumeheader[
    firstname=YourName,
    email=your.email@domain.com,
    linkedin=yourprofile,
    github=yourusername
]

% Education
\degree{Degree Name}{University}{Year}{
    \achievement{Your achievement here}
}

% Experience
\experience{Company Name}{
    \role{Job Title}{Start - End}{
        \achievement{Your accomplishment}
    }
}

% Skills
\tech{Python} \tech{R} \tech{Machine Learning}

% Projects
\project{Project Name}{Year}{
    \achievement{Your project achievement}
}
```

## Tips

- Use `\achievement{}` for bullet points
- Use `\tech{}` for skills and technologies
- Use `\honors{}` for honors/distinctions in degrees
- Keep descriptions concise and achievement-oriented

## Your Resume is Ready! 🎉

Your customized resume has been created using the Expressive Resume template and compiled to PDF. Check out `expressive-resume/src/my_resume.pdf` to see your professional resume!
